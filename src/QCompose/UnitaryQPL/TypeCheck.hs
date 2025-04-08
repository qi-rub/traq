module QCompose.UnitaryQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, local, runReaderT)
import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.MonadHelpers
import QCompose.Prelude
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import QCompose.UnitaryQPL.Syntax

type CheckingCtx a costT = (OracleDecl a, [ProcDef a costT], TypingCtx a)
type TypeChecker a costT = ReaderT (CheckingCtx a costT) (Either String)

oracleDecl :: Lens' (CheckingCtx a costT) (OracleDecl a)
oracleDecl = _1

procDefs :: Lens' (CheckingCtx a costT) [ProcDef a costT]
procDefs = _2

typingCtx :: Lens' (CheckingCtx a costT) (TypingCtx a)
typingCtx = _3

verifyArgs :: (TypeCheckable a) => [Ident] -> [VarType a] -> TypeChecker a costT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    view $ typingCtx . Ctx.at x . singular _Just

  when (arg_tys /= tys) $
    throwError $
      "mismatched args: " <> show (arg_tys, tys)

unitarySignature :: (TypeCheckable a, Show costT) => Unitary a costT -> TypeChecker a costT [VarType a]
unitarySignature Toffoli = return [tbool, tbool, tbool]
unitarySignature CNOT = return [tbool, tbool]
unitarySignature XGate = return [tbool]
unitarySignature HGate = return [tbool]
unitarySignature (Unif ty) = return [ty]
unitarySignature (UnifDagger ty) = return [ty]
unitarySignature Oracle = view (oracleDecl . to param_types)
unitarySignature (RevEmbedU f) = return $ revFunTys f
 where
  revFunTys ConstF{ty} = [ty]
  revFunTys NotF{ty} = [ty, ty]
  revFunTys IdF{ty} = [ty, ty]
  revFunTys AddF{ty} = [ty, ty, ty]
  revFunTys LEqF{ty} = [ty, ty, tbool]
unitarySignature (BlackBoxU bb) = throwError $ "cannot find signature for blackbox: " <> show bb
unitarySignature (Controlled u) = (tbool :) <$> unitarySignature u
unitarySignature (Refl0 ty) = return [ty]

typeCheckStmt :: (TypeCheckable a, Show costT) => Stmt a costT -> TypeChecker a costT ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt UnitaryS{unitary = BlackBoxU _} = return ()
typeCheckStmt UnitaryS{unitary, args} = do
  tys <- unitarySignature unitary
  verifyArgs args tys
typeCheckStmt CallS{proc_id, args} = do
  proc_param_tys <-
    view procDefs
      <&> filter ((proc_id ==) . proc_name)
      <&> head
      <&> proc_params
      <&> map (^. _3)
  verifyArgs args proc_param_tys
-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt' ss

typeCheckStmt' :: (TypeCheckable a, Show costT) => Stmt a costT -> TypeChecker a costT ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` ("typecheck failed: " <> show s)

typeCheckProc :: (TypeCheckable a, Show costT) => ProcDef a costT -> TypeChecker a costT ()
typeCheckProc procdef@ProcDef{proc_params, proc_body} =
  local (typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $
    typeCheckStmt' proc_body
      `throwFrom` ("typecheck proc failed: " <> show procdef)
 where
  withoutTag (x, _, ty) = (x, ty)

typeCheckProgram :: (TypeCheckable a, Show costT) => TypingCtx a -> Program a costT -> Either String ()
typeCheckProgram gamma Program{oracle_decl, proc_defs, stmt} = do
  let ctx = (oracle_decl, proc_defs, Ctx.empty)

  forM_ proc_defs $ (`runReaderT` ctx) . typeCheckProc

  runReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

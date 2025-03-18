module QCompose.UnitaryQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, local, runReaderT)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Basic
import QCompose.ProtoLang (OracleDecl (..), TypeCheckable (..), TypingCtx, VarType)
import QCompose.UnitaryQPL.Syntax
import QCompose.Utils.MonadHelpers

type CheckingCtx a = (OracleDecl a, [ProcDef a], TypingCtx a)
type TypeChecker a = ReaderT (CheckingCtx a) (Either String)

oracleDecl :: Lens' (CheckingCtx a) (OracleDecl a)
oracleDecl = _1

procDefs :: Lens' (CheckingCtx a) [ProcDef a]
procDefs = _2

typingCtx :: Lens' (CheckingCtx a) (TypingCtx a)
typingCtx = _3

verifyArgs :: (TypeCheckable a) => [Ident] -> [VarType a] -> TypeChecker a ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    view $ typingCtx . at x . singular _Just

  when (arg_tys /= tys) $
    throwError $ "mismatched args: " <> show (arg_tys, tys)

unitarySignature :: (TypeCheckable a) => Unitary a -> TypeChecker a [VarType a]
unitarySignature Toffoli = return [tbool, tbool, tbool]
unitarySignature CNOT = return [tbool, tbool]
unitarySignature Oracle = magnify oracleDecl $ do
  (++) <$> view (to param_types) <*> view (to ret_types)
unitarySignature (RevEmbedU f) = return $ revFunTys f
 where
  revFunTys ConstF{ty} = [ty]
  revFunTys NotF{ty} = [ty, ty]
  revFunTys IdF{ty} = [ty, ty]
  revFunTys AddF{ty} = [ty, ty, ty]
  revFunTys LEqF{ty} = [ty, ty, tbool]
unitarySignature (BlackBox b) = throwError $ "cannot find signature for blackbox: " <> b

typeCheckStmt :: (TypeCheckable a) => Stmt a -> TypeChecker a ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt UnitaryS{unitary = BlackBox _} = return ()
typeCheckStmt UnitaryS{unitary, args} = do
  tys <- unitarySignature unitary
  verifyArgs args tys
typeCheckStmt CallS{proc_id, args} = do
  proc_param_tys <-
    view procDefs
      <&> filter ((proc_id ==) . proc_name)
      <&> head
      <&> proc_params
      <&> map snd
  verifyArgs args proc_param_tys
-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt' ss

typeCheckStmt' :: (TypeCheckable a) => Stmt a -> TypeChecker a ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` ("typecheck failed: " <> show s)

typeCheckProc :: (TypeCheckable a) => ProcDef a -> TypeChecker a ()
typeCheckProc procdef@ProcDef{proc_params, proc_body} =
  local (typingCtx .~ M.fromList proc_params) $
    typeCheckStmt' proc_body
      `throwFrom` ("typecheck proc failed: " <> show procdef)

typeCheckProgram :: (TypeCheckable a) => TypingCtx a -> Program a -> Either String ()
typeCheckProgram gamma Program{oracle_decl, proc_defs, stmt} = do
  let ctx = (oracle_decl, proc_defs, M.empty)

  forM_ proc_defs $ (`runReaderT` ctx) . typeCheckProc

  runReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

module QCompose.UnitaryQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.Monad
import QCompose.Prelude
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import QCompose.UnitaryQPL.Syntax

type CheckingCtx sizeT costT = (ProcCtx sizeT costT, TypingCtx sizeT)
type TypeChecker sizeT costT = MyReaderT (CheckingCtx sizeT costT) (Either String)

procDefs :: Lens' (CheckingCtx sizeT costT) (ProcCtx sizeT costT)
procDefs = _1

typingCtx :: Lens' (CheckingCtx sizeT costT) (TypingCtx sizeT)
typingCtx = _2

verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker sizeT costT ()
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
unitarySignature (LoadData f) = do
  proc_def <- view (procDefs . Ctx.at f) >>= maybeWithError "cannot find function"
  return $ proc_def ^.. to proc_params . traverse . _3

typeCheckStmt :: (TypeCheckable a, Show costT) => Stmt a costT -> TypeChecker a costT ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt UnitaryS{unitary = BlackBoxU _} = return ()
typeCheckStmt UnitaryS{unitary, args} = do
  tys <- unitarySignature unitary
  verifyArgs args tys
typeCheckStmt CallS{proc_id, args} = do
  proc_param_tys <-
    view $
      procDefs
        . Ctx.at proc_id
        . singular _Just
        . to proc_params
        . to (map $ view _3)
  verifyArgs args proc_param_tys
-- compound statements
typeCheckStmt (RepeatS _ body) = typeCheckStmt body
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt' ss
typeCheckStmt (HoleS _) = throwError "cannot typecheck program with holes"

typeCheckStmt' :: (TypeCheckable a, Show costT) => Stmt a costT -> TypeChecker a costT ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` ("typecheck failed: " <> show s)

typeCheckProc :: (TypeCheckable a, Show costT) => ProcDef a costT -> TypeChecker a costT ()
typeCheckProc procdef@ProcDef{proc_params, mproc_body = Just proc_body} =
  local (typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $
    typeCheckStmt' proc_body
      `throwFrom` ("typecheck proc failed: " <> show procdef)
 where
  withoutTag (x, _, ty) = (x, ty)
typeCheckProc ProcDef{mproc_body = Nothing} = return ()

typeCheckProgram :: (TypeCheckable a, Show costT) => TypingCtx a -> Program a costT -> Either String ()
typeCheckProgram gamma Program{proc_defs, stmt} = do
  let ctx = (proc_defs, Ctx.empty)

  forM_ proc_defs $ (`runMyReaderT` ctx) . typeCheckProc

  runMyReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

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

type CheckingCtx holeT sizeT costT = (ProcCtx holeT sizeT costT, TypingCtx sizeT)
type TypeChecker holeT sizeT costT = MyReaderT (CheckingCtx holeT sizeT costT) (Either String)

procDefs :: Lens' (CheckingCtx holeT sizeT costT) (ProcCtx holeT sizeT costT)
procDefs = _1

typingCtx :: Lens' (CheckingCtx holeT sizeT costT) (TypingCtx sizeT)
typingCtx = _2

verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    view $ typingCtx . Ctx.at x . singular _Just

  when (arg_tys /= tys) $
    throwError $
      "mismatched args: " <> show (arg_tys, tys)

unitarySignature :: (TypeCheckable sizeT, Show costT) => Unitary sizeT costT -> TypeChecker holeT sizeT costT [VarType sizeT]
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
unitarySignature (Controlled u) = (tbool :) <$> unitarySignature u
unitarySignature (Refl0 ty) = return [ty]
unitarySignature (LoadData f) = do
  proc_def <- view (procDefs . Ctx.at f) >>= maybeWithError "cannot find function"
  return $ proc_def ^.. to proc_params . traverse . _3

typeCheckStmt :: (Show holeT, TypeCheckable sizeT, Show costT) => Stmt holeT sizeT costT -> TypeChecker holeT sizeT costT ()
-- single statements
typeCheckStmt SkipS = return ()
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
typeCheckStmt HoleS{} = return ()

typeCheckStmt' :: (Show holeT, TypeCheckable sizeT, Show costT) => Stmt holeT sizeT costT -> TypeChecker holeT sizeT costT ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` ("typecheck failed: " <> show s)

typeCheckProc :: (Show holeT, TypeCheckable sizeT, Show costT) => ProcDef holeT sizeT costT -> TypeChecker holeT sizeT costT ()
typeCheckProc procdef@ProcDef{proc_params, mproc_body = Just proc_body} =
  local (typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $
    typeCheckStmt' proc_body
      `throwFrom` ("typecheck proc failed: " <> show procdef)
 where
  withoutTag (x, _, ty) = (x, ty)
typeCheckProc ProcDef{mproc_body = Nothing} = return ()

typeCheckProgram :: (Show holeT, TypeCheckable sizeT, Show costT) => TypingCtx sizeT -> Program holeT sizeT costT -> Either String ()
typeCheckProgram gamma Program{proc_defs, stmt} = do
  let ctx = (proc_defs, Ctx.empty)

  forM_ proc_defs $ (`runMyReaderT` ctx) . typeCheckProc

  runMyReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

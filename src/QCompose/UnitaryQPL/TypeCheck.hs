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
import QCompose.Data.Errors

import QCompose.Control.Monad
import QCompose.Prelude
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import QCompose.UnitaryQPL.Syntax
import Text.Printf (printf)

type CheckingCtx holeT sizeT = (ProcCtx holeT sizeT, TypingCtx sizeT)
type TypeChecker holeT sizeT = MyReaderT (CheckingCtx holeT sizeT) (Either MyError)

procDefs :: Lens' (CheckingCtx holeT sizeT) (ProcCtx holeT sizeT)
procDefs = _1

typingCtx :: Lens' (CheckingCtx holeT sizeT) (TypingCtx sizeT)
typingCtx = _2

verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker holeT sizeT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ typingCtx . Ctx.at x
    maybeWithError (MessageE $ printf "cannot find argument %s" x) mty

  when (arg_tys /= tys) $
    throwError $
      MessageE $
        printf "mismatched args: expected %s, got %s" (show tys) (show arg_tys)

unitarySignature :: forall holeT sizeT. (TypeCheckable sizeT) => Unitary sizeT -> TypeChecker holeT sizeT [VarType sizeT]
unitarySignature Toffoli = return [tbool, tbool, tbool]
unitarySignature CNOT = return [tbool, tbool]
unitarySignature XGate = return [tbool]
unitarySignature HGate = return [tbool]
unitarySignature (Unif ty) = return [ty]
unitarySignature (UnifDagger ty) = return [ty]
unitarySignature (RevEmbedU f) = return $ revFunTys f
 where
  revFunTys :: ClassicalFun sizeT -> [VarType sizeT]
  revFunTys ConstF{ty} = [ty]
  revFunTys NotF{ty} = [ty, ty]
  revFunTys IdF{ty} = [ty, ty]
  revFunTys AddF{ty} = [ty, ty, ty]
  revFunTys LEqF{ty} = [ty, ty, tbool]
  revFunTys LEqConstF{ty} = [ty, tbool]
  revFunTys MultiOrF{cfun_n_args} = replicate (irange cfun_n_args + 1) tbool
unitarySignature (Controlled u) = (tbool :) <$> unitarySignature u
unitarySignature (Refl0 ty) = return [ty]
unitarySignature (LoadData f) = do
  proc_def <- view (procDefs . Ctx.at f) >>= maybeWithError (MessageE "cannot find function")
  return $ proc_def ^.. to proc_params . traverse . _3

typeCheckStmt :: (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt (CommentS _) = return ()
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
typeCheckStmt ForInRangeS{loop_body} = typeCheckStmt loop_body

typeCheckStmt' :: (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` MessageE ("typecheck failed: " <> show s)

typeCheckProc :: (Show holeT, TypeCheckable sizeT) => ProcDef holeT sizeT -> TypeChecker holeT sizeT ()
typeCheckProc procdef@ProcDef{proc_params, mproc_body = Just proc_body} =
  local (typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $
    typeCheckStmt' proc_body
      `throwFrom` MessageE ("typecheck proc failed: " <> show procdef)
 where
  withoutTag (x, _, ty) = (x, ty)
typeCheckProc ProcDef{mproc_body = Nothing} = return ()

typeCheckProgram :: (Show holeT, TypeCheckable sizeT) => TypingCtx sizeT -> Program holeT sizeT -> Either MyError ()
typeCheckProgram gamma Program{proc_defs, stmt} = do
  let ctx = (proc_defs, Ctx.empty)

  forM_ proc_defs $ (`runMyReaderT` ctx) . typeCheckProc

  runMyReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

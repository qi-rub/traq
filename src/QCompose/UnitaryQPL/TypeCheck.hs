module QCompose.UnitaryQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (local, runReaderT)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Errors as Err

import QCompose.Control.Monad
import QCompose.Prelude
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import qualified QCompose.ProtoLang as P
import QCompose.UnitaryQPL.Syntax
import Text.Printf (printf)

type CheckingCtx holeT sizeT costT = (ProcCtx holeT sizeT costT, TypingCtx sizeT)
type TypeChecker holeT sizeT costT = MyReaderT (CheckingCtx holeT sizeT costT) (Either Err.MyError)

procDefs :: Lens' (CheckingCtx holeT sizeT costT) (ProcCtx holeT sizeT costT)
procDefs = _1

typingCtx :: Lens' (CheckingCtx holeT sizeT costT) (TypingCtx sizeT)
typingCtx = _2

verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty

  when (arg_tys /= tys) $
    Err.throwErrorMessage $
      printf "mismatched args: expected %s, got %s" (show tys) (show arg_tys)

unitarySignature :: forall holeT costT sizeT. (TypeCheckable sizeT) => Unitary sizeT -> [VarType sizeT] -> TypeChecker holeT sizeT costT [VarType sizeT]
unitarySignature Toffoli _ = return [tbool, tbool, tbool]
unitarySignature CNOT _ = return [tbool, tbool]
unitarySignature XGate _ = return [tbool]
unitarySignature HGate _ = return [tbool]
unitarySignature (Unif ty) _ = return [ty]
unitarySignature (UnifDagger ty) _ = return [ty]
unitarySignature (RevEmbedU xs e) arg_tys = do
  let in_tys = take (length xs) arg_tys
  let gamma = Ctx.fromList $ zip xs in_tys
  let res = runReaderT (P.checkBasicExpr e) gamma
  case res of
    Left err -> Err.throwErrorMessage err
    Right ret_ty -> return $ in_tys ++ [ret_ty]
unitarySignature (Controlled u) arg_tys = (tbool :) <$> unitarySignature u (tail arg_tys)
unitarySignature (Refl0 ty) _ = return [ty]
unitarySignature (LoadData f) _ = do
  proc_def <- view (procDefs . Ctx.at f) >>= maybeWithError (Err.MessageE "cannot find function")
  return $ proc_def ^.. to proc_params . traverse . _3

typeCheckStmt :: (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT costT ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt (CommentS _) = return ()
typeCheckStmt UnitaryS{unitary, args} = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
  tys <- unitarySignature unitary arg_tys
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

typeCheckStmt' :: (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT costT ()
typeCheckStmt' s = typeCheckStmt s `throwFrom` Err.MessageE ("typecheck failed: " <> show s)

typeCheckProc :: (Show holeT, TypeCheckable sizeT, Show costT) => ProcDef holeT sizeT costT -> TypeChecker holeT sizeT costT ()
-- definition with a body
typeCheckProc procdef@ProcDef{proc_params, proc_body_or_tick = Right proc_body} =
  local (typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $
    typeCheckStmt' proc_body
      `throwFrom` Err.MessageE ("typecheck proc failed: " <> show procdef)
 where
  withoutTag (x, _, ty) = (x, ty)
-- declaration with a tick
typeCheckProc ProcDef{proc_body_or_tick = Left _} = return ()

typeCheckProgram :: (Show holeT, TypeCheckable sizeT, Show costT) => TypingCtx sizeT -> Program holeT sizeT costT -> Either Err.MyError ()
typeCheckProgram gamma Program{proc_defs, stmt} = do
  let ctx = (proc_defs, Ctx.empty)

  forM_ proc_defs $ (`runMyReaderT` ctx) . typeCheckProc

  runMyReaderT (typeCheckStmt' stmt) $ ctx & typingCtx .~ gamma

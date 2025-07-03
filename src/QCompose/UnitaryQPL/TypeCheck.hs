module QCompose.UnitaryQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
  _procCtx,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State (execStateT)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import QCompose.Data.Default
import qualified QCompose.Data.Errors as Err

import QCompose.Prelude
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import qualified QCompose.ProtoLang as P
import QCompose.UnitaryQPL.Syntax
import Text.Printf (printf)

-- ================================================================================
-- Context
-- ================================================================================
data CheckingCtx holeT sizeT costT = CheckingCtx (ProcCtx holeT sizeT costT) (TypingCtx sizeT)

instance HasDefault (CheckingCtx holeT sizeT costT) where
  default_ = CheckingCtx default_ default_

type instance P.SizeType (CheckingCtx holeT sizeT costT) = sizeT
type instance P.CostType (CheckingCtx holeT sizeT costT) = costT

_procCtx :: Lens' (CheckingCtx holeT sizeT costT) (ProcCtx holeT sizeT costT)
_procCtx focus (CheckingCtx p t) = focus p <&> \p' -> CheckingCtx p' t

instance P.HasTypingCtx (CheckingCtx holeT sizeT costT) where
  _typingCtx focus (CheckingCtx p t) = focus t <&> CheckingCtx p

-- ================================================================================
-- Typechecker
-- ================================================================================
type TypeChecker holeT sizeT costT = MyReaderT (CheckingCtx holeT sizeT costT) (Either Err.MyError)

verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty

  when (length arg_tys /= length tys) $
    Err.throwErrorMessage $
      printf "mismatched number of args: inferred %d, actual %d" (length tys) (length arg_tys)

  when (arg_tys /= tys) $
    Err.throwErrorMessage $
      printf "mismatched args: inferred %s, actual %s" (show tys) (show arg_tys)

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
  -- TODO use separate context for metaparams
  gamma' <- execStateT ?? gamma $ do
    all_gamma <- view $ P._typingCtx . to Ctx.toList
    forM all_gamma $ \(x, ty) ->
      when (head x == '#') $
        Ctx.ins x .= ty
  let res = runReaderT (P.checkBasicExpr e) gamma'
  case res of
    Left err -> Err.throwErrorMessage err
    Right ret_ty -> return $ in_tys ++ [ret_ty]
unitarySignature (Controlled u) arg_tys = (tbool :) <$> unitarySignature u (tail arg_tys)
unitarySignature (Refl0 ty) _ = return [ty]
unitarySignature (LoadData f) _ = do
  proc_def <- view (_procCtx . Ctx.at f) >>= maybeWithError (Err.MessageE "cannot find function")
  return $ proc_def ^.. to proc_params . traverse . _3

typeCheckStmt :: forall holeT sizeT costT. (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT costT ()
-- single statements
typeCheckStmt SkipS = return ()
typeCheckStmt HoleS{} = return ()
typeCheckStmt (CommentS _) = return ()
typeCheckStmt UnitaryS{unitary, args} = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
  tys <- unitarySignature unitary arg_tys
  verifyArgs args tys
typeCheckStmt CallS{proc_id, args} = do
  proc_param_tys <-
    view $
      _procCtx
        . Ctx.at proc_id
        . singular _Just
        . to proc_params
        . to (map $ view _3)
  verifyArgs args proc_param_tys
-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt' ss
typeCheckStmt (RepeatS _ body) = typeCheckStmt' body
typeCheckStmt ForInRangeS{iter_meta_var, iter_lim, loop_body} = do
  let iter_lim_ty = case iter_lim of
        P.MetaSize n -> P.Fin n
        _ -> error "unsupported loop limit"
  local (P._typingCtx . Ctx.ins ('#' : iter_meta_var) .~ iter_lim_ty) $ do
    typeCheckStmt' loop_body

typeCheckStmt' :: (Show holeT, TypeCheckable sizeT) => Stmt holeT sizeT -> TypeChecker holeT sizeT costT ()
typeCheckStmt' s = do
  gamma <- view P._typingCtx
  typeCheckStmt s
    `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
    `throwFrom` Err.MessageE (printf "typecheck failed: %s" (show s))

typeCheckProc :: (Show holeT, TypeCheckable sizeT, Show costT) => ProcDef holeT sizeT costT -> TypeChecker holeT sizeT costT ()
-- definition with a body
typeCheckProc procdef@ProcDef{proc_params, proc_body_or_tick = Right proc_body} =
  local (P._typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $ do
    gamma <- view P._typingCtx
    typeCheckStmt' proc_body
      `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
      `throwFrom` Err.MessageE (printf "typecheck proc failed: %s" (show procdef))
 where
  withoutTag (x, _, ty) = (x, ty)
-- declaration with a tick
typeCheckProc ProcDef{proc_body_or_tick = Left _} = return ()

typeCheckProgram :: (Show holeT, TypeCheckable sizeT, Show costT) => TypingCtx sizeT -> Program holeT sizeT costT -> Either Err.MyError ()
typeCheckProgram gamma Program{proc_defs, stmt} = do
  let ctx = default_ & (_procCtx .~ proc_defs)

  forM_ proc_defs $ (`runMyReaderT` ctx) . typeCheckProc

  runMyReaderT (typeCheckStmt' stmt) $ ctx & P._typingCtx .~ gamma

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Traq.CQPL.TypeCheck (
  -- * Monad
  CheckingCtx,
  TypeChecker,

  -- * Checking
  typeCheckUStmt,
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,
) where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader, ReaderT, local, runReaderT)
import Control.Monad.State (execStateT)
import Data.List (intersect)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Errors as Err

import Traq.CQPL.Syntax
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Helpers
-- ================================================================================
ensureSameLength ::
  (Foldable t, Foldable t', Show (t a), Show (t' b), MonadError Err.MyError m) =>
  t a ->
  t' b ->
  String ->
  m ()
ensureSameLength expected actual err = do
  when (length expected /= length actual) $ do
    Err.throwErrorMessage $ printf "%s: expected %s, got %s" err (show expected) (show actual)

ensureEqual :: (Show a, Eq a, MonadError Err.MyError m) => a -> a -> String -> m ()
ensureEqual expected actual err = do
  when (expected /= actual) $ do
    Err.throwErrorMessage $ printf "%s: expected %s, got %s" err (show expected) (show actual)

-- | Verify that the argument types match the deduced types
verifyArgTys ::
  forall sizeT m.
  (P.TypingReqs sizeT, MonadError Err.MyError m) =>
  [P.VarType sizeT] ->
  [P.VarType sizeT] ->
  m ()
verifyArgTys arg_tys tys = do
  when (length arg_tys /= length tys) $
    Err.throwErrorMessage $
      printf "mismatched number of args: inferred %d, actual %d" (length tys) (length arg_tys)

  when (arg_tys /= tys) $
    Err.throwErrorMessage $
      printf "mismatched args: inferred %s, actual %s" (show tys) (show arg_tys)

getArgTy ::
  forall size env m.
  ( P.TypingReqs size
  , MonadError Err.MyError m
  , MonadReader env m
  , P.HasTypingCtx env
  , size ~ SizeType env
  ) =>
  Arg size ->
  m (P.VarType size)
getArgTy (Arg x) = do
  mty <- view $ P._typingCtx . Ctx.at x
  maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
getArgTy (ArrElemArg arg _) = do
  ty <- getArgTy arg
  case ty of
    P.Arr _ e_ty -> pure e_ty
    P.Bitvec _ -> pure P.tbool
    P.Tup _ -> error "TODO tuple index"
    _ -> throwError (Err.MessageE $ printf "expected an array/tuple type")

-- | Verify that the arguments match the deduced types
verifyArgs ::
  forall sizeT env m.
  ( P.TypingReqs sizeT
  , MonadError Err.MyError m
  , MonadReader env m
  , P.HasTypingCtx env
  , sizeT ~ SizeType env
  ) =>
  [Arg sizeT] ->
  [P.VarType sizeT] ->
  m ()
verifyArgs args tys = do
  arg_tys <- forM args getArgTy
  verifyArgTys arg_tys tys

-- ================================================================================
-- Type-checking Context
-- ================================================================================

-- | Env for type checking
data CheckingCtx sizeT = CheckingCtx (ProcCtx sizeT) (P.TypingCtx sizeT)
  deriving (Generic, HasDefault)

type instance SizeType (CheckingCtx sizeT) = sizeT

instance HasProcCtx (CheckingCtx sizeT) where
  _procCtx focus (CheckingCtx p t) = focus p <&> \p' -> CheckingCtx p' t

instance P.HasTypingCtx (CheckingCtx sizeT) where
  _typingCtx focus (CheckingCtx p t) = focus t <&> \t' -> CheckingCtx p t'

-- | Monad for type checking
type TypeChecker sizeT = ReaderT (CheckingCtx sizeT) (Either Err.MyError)

-- ================================================================================
-- Type Checking
-- ================================================================================

typeCheckBasicGate :: forall size. (P.TypingReqs size) => BasicGate -> [P.VarType size] -> TypeChecker size ()
typeCheckBasicGate Toffoli tys = verifyArgTys tys [P.tbool, P.tbool, P.tbool]
typeCheckBasicGate CNOT tys = verifyArgTys tys [P.tbool, P.tbool]
typeCheckBasicGate XGate tys = verifyArgTys tys [P.tbool]
typeCheckBasicGate HGate tys = verifyArgTys tys [P.tbool]
typeCheckBasicGate ZGate tys = verifyArgTys tys [P.tbool]
typeCheckBasicGate (Rz _) tys = verifyArgTys tys [P.tbool]
typeCheckBasicGate (PhaseOnZero _) _ = return ()
typeCheckBasicGate COPY tys = let n = length tys `div` 2 in verifyArgTys (take n tys) (drop n tys)
typeCheckBasicGate SWAP tys = let n = length tys `div` 2 in verifyArgTys (take n tys) (drop n tys)

typeCheckUnitary :: forall sizeT. (P.TypingReqs sizeT) => Unitary sizeT -> [P.VarType sizeT] -> TypeChecker sizeT ()
typeCheckUnitary (BasicGateU g) tys = typeCheckBasicGate g tys
typeCheckUnitary (DistrU (P.UniformE ty)) tys = verifyArgTys tys [ty]
typeCheckUnitary (DistrU (P.BernoulliE _)) tys = verifyArgTys tys [P.tbool]
typeCheckUnitary (RevEmbedU xs e) tys = do
  let in_tys = take (length xs) tys
  let gamma = Ctx.fromList $ zip xs in_tys
  -- TODO use separate context for metaparams
  gamma' <- execStateT ?? gamma $ do
    all_gamma <- view $ P._typingCtx . to Ctx.toList
    forM all_gamma $ \(x, ty) ->
      when (head x == '#') $
        Ctx.ins x .= ty
  let res = runReaderT (P.typeCheckBasicExpr e) gamma'
  case res of
    Left err -> Err.throwErrorMessage err
    Right ret_ty -> verifyArgTys (drop (length xs) tys) [ret_ty]
-- composite gates
typeCheckUnitary (Controlled u) tys = do
  verifyArgTys [head tys] [P.tbool]
  typeCheckUnitary u (tail tys)
typeCheckUnitary (Adjoint u) tys = typeCheckUnitary u tys

typeCheckUStmt :: forall sizeT. (P.TypingReqs sizeT) => UStmt sizeT -> TypeChecker sizeT ()
-- single statements
typeCheckUStmt USkipS = return ()
typeCheckUStmt (UCommentS _) = return ()
typeCheckUStmt UnitaryS{unitary, qargs} = do
  arg_tys <- forM qargs getArgTy
  typeCheckUnitary unitary arg_tys
typeCheckUStmt UCallS{uproc_id, qargs} = do
  ProcDef{proc_param_types, proc_body} <-
    view (_procCtx . Ctx.at uproc_id)
      >>= singularM _Just (Err.MessageE $ printf "cannot find procedure `%s`" uproc_id)
  unless (isUProc proc_body) $ Err.throwErrorMessage "expected uproc"

  verifyArgs qargs proc_param_types
-- compound statements
typeCheckUStmt (USeqS ss) = mapM_ typeCheckUStmt' ss
typeCheckUStmt (URepeatS _ body) = typeCheckUStmt' body
typeCheckUStmt UForInRangeS{iter_meta_var, iter_lim, uloop_body} = do
  let iter_lim_ty = case iter_lim of
        P.MetaSize n -> P.Fin n
        _ -> error "unsupported loop limit"
  local (P._typingCtx . Ctx.ins ('#' : iter_meta_var) .~ iter_lim_ty) $ do
    typeCheckUStmt' uloop_body
typeCheckUStmt UWithComputedS{with_ustmt, body_ustmt} = mapM_ typeCheckUStmt' [with_ustmt, body_ustmt]

typeCheckUStmt' :: (P.TypingReqs sizeT) => UStmt sizeT -> TypeChecker sizeT ()
typeCheckUStmt' s = do
  gamma <- view P._typingCtx
  typeCheckUStmt s
    `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
    `throwFrom` Err.MessageE (printf "typecheck failed: %s" (show s))

-- | Check a statement
typeCheckStmt ::
  forall sizeT.
  (P.TypingReqs sizeT) =>
  Stmt sizeT ->
  TypeChecker sizeT ()
typeCheckStmt SkipS = return ()
typeCheckStmt (CommentS _) = return ()
-- Simple statements
typeCheckStmt AssignS{rets, expr} = do
  gamma <- view P._typingCtx
  actual_ret_tys <-
    case runReaderT ?? gamma $ P.typeCheckBasicExpr expr of
      Left err -> Err.throwErrorMessage err
      Right ty -> return [ty]

  expect_ret_tys <- forM rets $ \var -> do
    view (P._typingCtx . Ctx.at var)
      >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)

  when (expect_ret_tys /= actual_ret_tys) $ do
    Err.throwErrorMessage $
      printf
        "mismatched expression return types: expected %s, got %s"
        (show expect_ret_tys)
        (show actual_ret_tys)
typeCheckStmt RandomS{} = return ()
typeCheckStmt RandomDynS{max_var} = do
  view (P._typingCtx . Ctx.at max_var) >>= maybeWithError (Err.MessageE $ printf "cannot find variable %s" max_var)
  return ()

-- call proc
typeCheckStmt CallS{fun = FunctionCall proc_id, args} = do
  p@ProcDef{proc_param_types} <- do
    view (_procCtx . Ctx.at proc_id)
      >>= maybeWithError (Err.MessageE "cannot find proc")
  unless (isCProc p) $ Err.throwErrorMessage "expected uproc"
  verifyArgs args proc_param_types

-- call uproc
typeCheckStmt CallS{fun = UProcAndMeas uproc_id, args} = do
  p@ProcDef{proc_param_types} <- do
    view (_procCtx . Ctx.at uproc_id)
      >>= maybeWithError (Err.MessageE $ printf "cannot find proc %s" uproc_id)

  unless (isUProc p) $ Err.throwErrorMessage "expected uproc"

  arg_tys <- forM args getArgTy
  ensureEqual (take (length arg_tys) proc_param_types) arg_tys ("mismatched function args for meas uproc " ++ uproc_id)

-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt ss
typeCheckStmt IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- view (P._typingCtx . Ctx.at cond) >>= maybeWithError (Err.MessageE $ "cannot find variable " ++ cond)
  when (cond_ty /= P.tbool) $
    Err.throwErrorMessage $
      "if cond must be bool, got " <> show cond_ty
  typeCheckStmt s_true
  typeCheckStmt s_false
typeCheckStmt RepeatS{loop_body} = typeCheckStmt loop_body
typeCheckStmt ForInRangeS{loop_body, iter_meta_var, iter_lim} = do
  iter_ty <- case iter_lim of
    P.MetaSize n -> pure $ P.Fin n
    P.MetaValue n -> pure $ P.Fin $ fromIntegral n
    P.MetaName _ -> Err.throwErrorMessage "cannot find iteration"
  local (P._typingCtx . Ctx.ins ('#' : iter_meta_var) .~ iter_ty) $
    typeCheckStmt loop_body
-- try by desugaring
typeCheckStmt s = case desugarS s of
  Just s' -> typeCheckStmt s'
  Nothing -> error $ "Unable to TypeCheck: " ++ show s

typeCheckUProcBody ::
  forall sizeT.
  (P.TypingReqs sizeT) =>
  UProcBody sizeT ->
  -- | parameter types
  [P.VarType sizeT] ->
  TypeChecker sizeT ()
-- declaration with a tick
typeCheckUProcBody UProcDecl{} _ = return ()
-- definition with a body
typeCheckUProcBody procdef@UProcBody{uproc_param_names, uproc_body_stmt} tys = do
  ensureSameLength uproc_param_names tys "types vs params"
  let gamma = Ctx.fromList $ zip uproc_param_names tys
  local (P._typingCtx .~ gamma) $ do
    typeCheckUStmt' uproc_body_stmt
      `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
      `throwFrom` Err.MessageE (printf "typecheck proc failed: %s" (show procdef))

-- | Check a procedure def
typeCheckCProcBody ::
  forall sizeT.
  (P.TypingReqs sizeT) =>
  CProcBody sizeT ->
  -- | parameter types
  [P.VarType sizeT] ->
  TypeChecker sizeT ()
-- declaration with a tick
typeCheckCProcBody CProcDecl{} _ = return ()
-- definition with a body
typeCheckCProcBody CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} tys = do
  ensureSameLength cproc_param_names tys "types vs params"

  let common_names = cproc_param_names `intersect` map fst cproc_local_vars
  unless (null common_names) $
    Err.throwErrorMessage $
      printf "clashing names in proc args and locals: %s" (show common_names)

  let gamma = Ctx.fromList $ zip cproc_param_names tys ++ cproc_local_vars
  local (P._typingCtx .~ gamma) $ do
    typeCheckStmt cproc_body_stmt

typeCheckProc ::
  forall sizeT.
  (P.TypingReqs sizeT) =>
  ProcDef sizeT ->
  TypeChecker sizeT ()
typeCheckProc ProcDef{proc_name, proc_param_types, proc_body} =
  case proc_body of
    ProcBodyC cbody -> typeCheckCProcBody cbody proc_param_types
    ProcBodyU ubody -> typeCheckUProcBody ubody proc_param_types
    `throwFrom` Err.MessageE (printf "failed typecheck proc: %s" proc_name)

-- | Check an entire program given the input bindings.
typeCheckProgram ::
  forall sizeT.
  (P.TypingReqs sizeT) =>
  Program sizeT ->
  Either Err.MyError ()
typeCheckProgram (Program ps) = do
  let env =
        default_
          & (_procCtx .~ Ctx.fromListWith proc_name ps)
  runReaderT ?? env $ do
    mapM_ typeCheckProc ps

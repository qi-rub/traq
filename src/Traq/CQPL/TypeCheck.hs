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
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, local, runReaderT)
import Control.Monad.State (execStateT)
import Data.Foldable (toList)
import Data.List (intersect)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

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
  (P.TypeCheckable sizeT, MonadError Err.MyError m) =>
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

-- | Verify that the arguments match the deduced types
verifyArgs ::
  forall sizeT env m.
  ( P.TypeCheckable sizeT
  , MonadError Err.MyError m
  , MonadReader env m
  , P.HasTypingCtx env
  , sizeT ~ SizeType env
  ) =>
  [Ident] ->
  [P.VarType sizeT] ->
  m ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
  verifyArgTys arg_tys tys

-- ================================================================================
-- Type-checking Context
-- ================================================================================

-- | Env for type checking
data CheckingCtx holeT sizeT costT
  = CheckingCtx
      (ProcCtx holeT sizeT costT)
      (P.TypingCtx sizeT)

type instance SizeType (CheckingCtx holeT sizeT costT) = sizeT
type instance CostType (CheckingCtx holeT sizeT costT) = costT
type instance HoleType (CheckingCtx holeT sizeT costT) = holeT

instance HasDefault (CheckingCtx holeT sizeT costT) where default_ = CheckingCtx default_ default_

instance HasProcCtx (CheckingCtx holeT sizeT costT) where
  _procCtx focus (CheckingCtx p t) = focus p <&> \p' -> CheckingCtx p' t

instance P.HasTypingCtx (CheckingCtx holeT sizeT costT) where
  _typingCtx focus (CheckingCtx p t) = focus t <&> \t' -> CheckingCtx p t'

-- | Monad for type checking
type TypeChecker holeT sizeT costT = ReaderT (CheckingCtx holeT sizeT costT) (Either Err.MyError)

-- ================================================================================
-- Type Checking
-- ================================================================================

typeCheckUnitary :: forall holeT costT sizeT. (P.TypeCheckable sizeT) => Unitary sizeT -> [P.VarType sizeT] -> TypeChecker holeT sizeT costT ()
typeCheckUnitary Toffoli tys = verifyArgTys tys [P.tbool, P.tbool, P.tbool]
typeCheckUnitary CNOT tys = verifyArgTys tys [P.tbool, P.tbool]
typeCheckUnitary XGate tys = verifyArgTys tys [P.tbool]
typeCheckUnitary HGate tys = verifyArgTys tys [P.tbool]
typeCheckUnitary Unif _ = return ()
typeCheckUnitary Refl0 _ = return ()
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
typeCheckUnitary (Controlled u) tys = do
  verifyArgTys [head tys] [P.tbool]
  typeCheckUnitary u (tail tys)
typeCheckUnitary (Adjoint u) tys = typeCheckUnitary u tys
typeCheckUnitary (LoadData f) tys = do
  proc_def@ProcDef{proc_param_types} <-
    view (_procCtx . Ctx.at f)
      >>= maybeWithError (Err.MessageE "cannot find function")
  unless (isUProc proc_def) $ Err.throwErrorMessage "expected uproc"

  verifyArgTys tys proc_param_types

typeCheckUStmt :: forall holeT sizeT costT. (Show holeT, P.TypeCheckable sizeT) => UStmt holeT sizeT -> TypeChecker holeT sizeT costT ()
-- single statements
typeCheckUStmt USkipS = return ()
typeCheckUStmt UHoleS{} = return ()
typeCheckUStmt (UCommentS _) = return ()
typeCheckUStmt UnitaryS{unitary, qargs} = do
  arg_tys <- forM qargs $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
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

typeCheckUStmt' :: (Show holeT, P.TypeCheckable sizeT) => UStmt holeT sizeT -> TypeChecker holeT sizeT costT ()
typeCheckUStmt' s = do
  gamma <- view P._typingCtx
  typeCheckUStmt s
    `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
    `throwFrom` Err.MessageE (printf "typecheck failed: %s" (show s))

-- | Check a statement
typeCheckStmt ::
  forall sizeT costT holeT.
  (P.TypeCheckable sizeT, Show holeT) =>
  Stmt holeT sizeT ->
  TypeChecker holeT sizeT costT ()
typeCheckStmt SkipS = return ()
typeCheckStmt (CommentS _) = return ()
-- ignore holes
typeCheckStmt (HoleS _) = return ()
-- Simple statements
typeCheckStmt AssignS{rets, expr} = do
  let expr_vars = toList $ P.freeVarsBE expr
  expr_var_tys <- forM expr_vars $ \var -> do
    view (P._typingCtx . Ctx.at var)
      >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)
  let gamma = Ctx.fromList $ zip expr_vars expr_var_tys

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
  arg_tys <- forM args $ \var -> do
    view (P._typingCtx . Ctx.at var) >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)
  ensureEqual proc_param_types arg_tys "mismatched function args"

-- call uproc
typeCheckStmt CallS{fun = UProcAndMeas uproc_id, args} = do
  p@ProcDef{proc_param_types} <- do
    view (_procCtx . Ctx.at uproc_id)
      >>= maybeWithError (Err.MessageE $ printf "cannot find proc %s" uproc_id)

  unless (isUProc p) $ Err.throwErrorMessage "expected uproc"

  arg_tys <- forM args $ \var -> do
    view (P._typingCtx . Ctx.at var) >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)
  ensureEqual (take (length arg_tys) proc_param_types) arg_tys "mismatched function args"

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
-- try by desugaring
typeCheckStmt s = case desugarS s of
  Just s' -> typeCheckStmt s'
  Nothing -> error $ "Unable to TypeCheck: " ++ show s

typeCheckUProcBody ::
  forall sizeT costT holeT.
  (P.TypeCheckable sizeT, Show costT, Show holeT) =>
  UProcBody holeT sizeT costT ->
  -- | parameter types
  [P.VarType sizeT] ->
  TypeChecker holeT sizeT costT ()
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
  forall sizeT costT holeT.
  (P.TypeCheckable sizeT, Show holeT) =>
  CProcBody holeT sizeT costT ->
  -- | parameter types
  [P.VarType sizeT] ->
  TypeChecker holeT sizeT costT ()
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
  forall sizeT costT holeT.
  (P.TypeCheckable sizeT, Show costT, Show holeT) =>
  ProcDef holeT sizeT costT ->
  TypeChecker holeT sizeT costT ()
typeCheckProc ProcDef{proc_param_types, proc_body} =
  case proc_body of
    ProcBodyC cbody -> typeCheckCProcBody cbody proc_param_types
    ProcBodyU ubody -> typeCheckUProcBody ubody proc_param_types

-- | Check an entire program given the input bindings.
typeCheckProgram ::
  forall sizeT costT holeT.
  (P.TypeCheckable sizeT, Show holeT, Show costT) =>
  Program holeT sizeT costT ->
  Either Err.MyError ()
typeCheckProgram Program{proc_defs} = do
  let env =
        default_
          & (_procCtx .~ proc_defs)
  runReaderT ?? env $ do
    mapM_ typeCheckProc $ Ctx.elems proc_defs

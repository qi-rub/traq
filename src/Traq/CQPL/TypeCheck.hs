module Traq.CQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
) where

import Control.Monad (forM, unless, when)
import Control.Monad.RWS (local)
import Control.Monad.Reader (runReaderT)
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
import Traq.ProtoLang (TypeCheckable (..))
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL

-- | Env for type checking
data CheckingCtx holeT sizeT costT = CheckingCtx (ProcCtx holeT sizeT costT) (UQPL.ProcCtx holeT sizeT costT) (P.TypingCtx sizeT)

type instance SizeType (CheckingCtx holeT sizeT costT) = sizeT
type instance CostType (CheckingCtx holeT sizeT costT) = costT
type instance HoleType (CheckingCtx holeT sizeT costT) = holeT

instance HasDefault (CheckingCtx holeT sizeT costT) where default_ = CheckingCtx default_ default_ default_

instance HasProcCtx (CheckingCtx holeT sizeT costT) where
  _procCtx focus (CheckingCtx p up t) = focus p <&> \p' -> CheckingCtx p' up t

instance P.HasTypingCtx (CheckingCtx holeT sizeT costT) where
  _typingCtx focus (CheckingCtx p up t) = focus t <&> \t' -> CheckingCtx p up t'

uProcs :: Lens' (CheckingCtx holeT sizeT costT) (UQPL.ProcCtx holeT sizeT costT)
uProcs = lens get' set'
 where
  get' (CheckingCtx _ u _) = u
  set' (CheckingCtx p _ t) u = CheckingCtx p u t

-- | Monad for type checking
type TypeChecker holeT sizeT costT = MyReaderT (CheckingCtx holeT sizeT costT) (Either Err.MyError)

ensureEqual :: (Show a, Eq a) => a -> a -> String -> TypeChecker holeT sizeT costT ()
ensureEqual expected actual err = do
  when (expected /= actual) $ do
    Err.throwErrorMessage $ printf "%s: expected %s, got %s" err (show expected) (show actual)

-- | Check a statement
typeCheckStmt ::
  forall sizeT costT holeT.
  (TypeCheckable sizeT, Show holeT) =>
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
-- function call
typeCheckStmt CallS{fun = FunctionCall proc_id, args} = do
  ProcDef{proc_param_types} <- magnify _procCtx $ do
    view (Ctx.at proc_id) >>= maybeWithError (Err.MessageE "cannot find proc")
  arg_tys <- forM args $ \var -> do
    view (P._typingCtx . Ctx.at var) >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)
  ensureEqual proc_param_types arg_tys "mismatched function args"
typeCheckStmt CallS{fun = UProcAndMeas uproc_id, args} = do
  UQPL.UProcDef{UQPL.proc_params} <- magnify uProcs $ do
    view (Ctx.at uproc_id) >>= maybeWithError (Err.MessageE $ printf "cannot find proc %s" uproc_id)
  arg_tys <- forM args $ \var -> do
    view (P._typingCtx . Ctx.at var) >>= maybeWithError (Err.MessageE $ printf "cannot find %s" var)
  let uproc_param_types = map (view _3) proc_params & take (length arg_tys)
  ensureEqual uproc_param_types arg_tys "mismatched function args"
-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt ss
typeCheckStmt IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- view (P._typingCtx . Ctx.at cond) >>= maybeWithError (Err.MessageE $ "cannot find variable " ++ cond)
  when (cond_ty /= tbool) $
    Err.throwErrorMessage $
      "if cond must be bool, got " <> show cond_ty
  typeCheckStmt s_true
  typeCheckStmt s_false
typeCheckStmt RepeatS{loop_body} = typeCheckStmt loop_body
-- try by desugaring
typeCheckStmt s = case desugarS s of
  Just s' -> typeCheckStmt s'
  Nothing -> error $ "Unable to TypeCheck: " ++ show s

-- | Check a procedure def
typeCheckProc ::
  forall sizeT costT holeT.
  (TypeCheckable sizeT, Show holeT) =>
  ProcDef holeT sizeT costT ->
  TypeChecker holeT sizeT costT ()
typeCheckProc ProcDef{proc_body_or_tick = Left _} = return ()
typeCheckProc
  ProcDef
    { proc_param_types
    , proc_body_or_tick = Right ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}
    } = do
    when (length proc_param_types /= length proc_param_names) $ do
      Err.throwErrorMessage $
        printf
          "mismatched number of proc args: expected types %s, got names %s"
          (show proc_param_types)
          (show proc_param_names)

    let common_names = proc_param_names `intersect` map fst proc_local_vars
    unless (null common_names) $
      Err.throwErrorMessage $
        printf "clashing names in proc args and locals: %s" (show common_names)

    let gamma = Ctx.fromList $ zip proc_param_names proc_param_types ++ proc_local_vars
    local (P._typingCtx .~ gamma) $ do
      typeCheckStmt proc_body_stmt

-- | Check an entire program given the input bindings.
typeCheckProgram ::
  forall sizeT costT holeT.
  (TypeCheckable sizeT, Show holeT, Show costT) =>
  Program holeT sizeT costT ->
  Either Err.MyError ()
typeCheckProgram Program{proc_defs, uproc_defs} = do
  let uqplTypingEnv = default_ & UQPL._procCtx .~ uproc_defs
  runMyReaderT ?? uqplTypingEnv $ do
    mapM_ UQPL.typeCheckProc $ Ctx.elems uproc_defs

  let env = default_ & (_procCtx .~ proc_defs) & (uProcs .~ uproc_defs)
  runMyReaderT ?? env $ do
    mapM_ typeCheckProc $ Ctx.elems proc_defs

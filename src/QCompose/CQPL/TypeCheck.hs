module QCompose.CQPL.TypeCheck (
  typeCheckStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (local)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import Data.List (intersect)
import QCompose.CQPL.Syntax
import QCompose.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import qualified QCompose.UnitaryQPL as UQPL

-- | Env for type checking
type CheckingCtx holeT sizeT = (ProcCtx holeT sizeT, UQPL.ProcCtx holeT sizeT, TypingCtx sizeT)

cqProcs :: Lens' (CheckingCtx holeT sizeT) (ProcCtx holeT sizeT)
cqProcs = _1

uProcs :: Lens' (CheckingCtx holeT sizeT) (UQPL.ProcCtx holeT sizeT)
uProcs = _2

typingCtx :: Lens' (CheckingCtx holeT sizeT) (TypingCtx sizeT)
typingCtx = _3

-- | Monad for type checking
type TypeChecker holeT sizeT = MyReaderT (CheckingCtx holeT sizeT) (Either String)

ensureEqual :: (Show a, Eq a) => a -> a -> String -> TypeChecker holeT sizeT ()
ensureEqual expected actual err = do
  when (expected /= actual) $ do
    throwError $ printf "%s: expected %s, got %s" err (show expected) (show actual)

ensureOne :: TypeChecker holeT sizeT [a] -> TypeChecker holeT sizeT a
ensureOne m = do
  xs <- m
  case xs of
    [x] -> return x
    _ -> throwError $ printf "expected one value, got %d" (show (length xs))

-- | Check an expression and return the return types
typeCheckExpr ::
  forall sizeT holeT.
  (TypeCheckable sizeT, Show holeT) =>
  Expr sizeT ->
  TypeChecker holeT sizeT [VarType sizeT]
typeCheckExpr ConstE{val_ty} = return [val_ty]
typeCheckExpr VarE{var} = do
  ty <- magnify typingCtx $ Ctx.lookup' var
  return [ty]
typeCheckExpr AddE{lhs, rhs} = do
  lhs_ty <- ensureOne $ typeCheckExpr lhs
  rhs_ty <- ensureOne $ typeCheckExpr rhs
  when (lhs_ty /= rhs_ty) $
    throwError $
      printf "Add: mismatched types %s, %s" (show lhs_ty) (show rhs_ty)
  return [lhs_ty]
typeCheckExpr LEqE{lhs, rhs} = do
  lhs_ty <- ensureOne $ typeCheckExpr lhs
  rhs_ty <- ensureOne $ typeCheckExpr rhs
  when (lhs_ty /= rhs_ty) $
    throwError $
      printf "LEqE: mismatched types %s, %s" (show lhs_ty) (show rhs_ty)
  return [tbool]
typeCheckExpr s = error $ "TODO typeCheckExpr: " <> show s

-- | Check a statement
typeCheckStmt ::
  forall sizeT holeT.
  (TypeCheckable sizeT, Show holeT) =>
  Stmt holeT sizeT ->
  TypeChecker holeT sizeT ()
typeCheckStmt SkipS = return ()
typeCheckStmt (CommentS _) = return ()
-- ignore holes
typeCheckStmt (HoleS _) = return ()
-- Simple statements
typeCheckStmt AssignS{rets, expr} = do
  expect_ret_tys <- magnify typingCtx $ mapM Ctx.lookup' rets
  actual_ret_tys <- typeCheckExpr expr
  when (expect_ret_tys /= actual_ret_tys) $ do
    throwError $
      printf
        "mismatched expression return types: expected %s, got %s"
        (show expect_ret_tys)
        (show actual_ret_tys)
typeCheckStmt RandomS{ret, ty} = do
  -- ret_ty <- magnify typingCtx $ Ctx.lookup' ret
  -- when (ret_ty /= ty) $ do
  --   throwError $ printf "random bound must match type, expected %s got %s" (show ret_ty) (show ty)
  return ()
-- function call
typeCheckStmt CallS{fun = FunctionCall proc_id, meta_params, args} = do
  ProcDef{proc_param_types} <- magnify cqProcs $ Ctx.lookup' proc_id
  arg_tys <- magnify typingCtx $ mapM Ctx.lookup' args
  ensureEqual proc_param_types arg_tys "mismatched function args"
typeCheckStmt CallS{fun = UProcAndMeas uproc_id, meta_params, args} = do
  UQPL.ProcDef{UQPL.proc_params, UQPL.proc_meta_params} <- magnify uProcs $ Ctx.lookup' uproc_id
  arg_tys <- magnify typingCtx $ mapM Ctx.lookup' args
  let uproc_param_types = map (view _3) proc_params & take (length arg_tys)
  ensureEqual uproc_param_types arg_tys "mismatched function args"
-- compound statements
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt ss
typeCheckStmt IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- magnify typingCtx $ Ctx.lookup' cond
  when (cond_ty /= tbool) $
    throwError $
      "if cond must be bool, got " <> show cond_ty
  typeCheckStmt s_true
  typeCheckStmt s_false
typeCheckStmt RepeatS{loop_body} = typeCheckStmt loop_body
typeCheckStmt s = error $ "TODO " ++ show s

-- | Check a procedure def
typeCheckProc ::
  forall sizeT holeT.
  (TypeCheckable sizeT, Show holeT) =>
  ProcDef holeT sizeT ->
  TypeChecker holeT sizeT ()
typeCheckProc ProcDef{mproc_body = Nothing} = return ()
typeCheckProc
  ProcDef
    { proc_meta_params
    , proc_param_types
    , mproc_body = Just ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}
    } = do
    when (length proc_param_types /= length proc_param_names) $ do
      throwError $
        printf
          "mismatched number of proc args: expected types %s, got names %s"
          (show proc_param_types)
          (show proc_param_names)

    let common_names = proc_param_names `intersect` map fst proc_local_vars
    unless (null common_names) $
      throwError $
        printf "clashing names in proc args and locals: %s" (show common_names)

    let gamma = Ctx.fromList $ zip proc_param_names proc_param_types ++ proc_local_vars
    local (typingCtx .~ gamma) $ do
      typeCheckStmt proc_body_stmt

-- | Check an entire program given the input bindings.
typeCheckProgram ::
  forall sizeT holeT.
  (TypeCheckable sizeT, Show holeT) =>
  TypingCtx sizeT ->
  Program holeT sizeT ->
  Either String ()
typeCheckProgram gamma Program{proc_defs, uproc_defs, stmt} = do
  flip runMyReaderT (uproc_defs, undefined) $ do
    mapM_ UQPL.typeCheckProc $ Ctx.elems uproc_defs

  flip runMyReaderT (proc_defs, uproc_defs, undefined) $ do
    mapM_ typeCheckProc $ Ctx.elems proc_defs

  flip runMyReaderT (proc_defs, uproc_defs, gamma) $ do
    typeCheckStmt stmt

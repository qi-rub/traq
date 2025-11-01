{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.ProtoLang.TypeCheck (
  -- * Typing Context
  TypingCtx,
  HasTypingCtx (..),

  -- * Typeable size types
  TypeCheckable,
  tbool,

  -- * Types
  TypingEnv,
  TypeChecker,
  TypeCheckablePrimitive (..),

  -- * Checkers
  typeCheckBasicExpr,
  typeCheckStmt,
  typeCheckFun,
  typeCheckProg,

  -- * Accessors
  lookupFunE,
) where

import Control.Monad (forM_, unless, when, zipWithM_)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, execStateT)
import Control.Monad.Trans (lift)
import GHC.Generics
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Typing Context
-- ================================================================================

-- | A context mapping variables to their types.
type TypingCtx sizeT = Ctx.Context (VarType sizeT)

class HasTypingCtx p where
  _typingCtx :: (sizeT ~ SizeType p) => Lens' p (TypingCtx sizeT)

instance HasTypingCtx (TypingCtx sizeT) where _typingCtx = id

-- ================================================================================
-- Typecheckable size types
-- ================================================================================

-- | Type @sizeT@ that can be type-checked.
type TypeCheckable sizeT = (Eq sizeT, Show sizeT, Num sizeT)

-- | Boolean type in our language.
tbool :: (Num sizeT) => VarType sizeT
tbool = Fin 2

-- ================================================================================
-- Typing inference for basic expressions
-- ================================================================================

typeCheckBasicExpr ::
  forall sizeT m.
  ( TypeCheckable sizeT
  , MonadError String m
  , MonadReader (TypingCtx sizeT) m
  ) =>
  BasicExpr sizeT ->
  m (VarType sizeT)
typeCheckBasicExpr VarE{var} = Ctx.lookup' var
typeCheckBasicExpr ParamE{param} = do
  -- TODO use a separate context for params
  -- throwError $ printf "unsupported: typechecking for parameters (got: #%s)" param
  -- gamma <- view id
  -- throwError $ printf "UNSUPPORTED #%s : %s" param (show gamma)
  Ctx.lookup' $ '#' : param
typeCheckBasicExpr DefaultE{ty} = return ty
typeCheckBasicExpr ConstE{ty} = return ty
typeCheckBasicExpr UnOpE{un_op, operand} = do
  arg_ty <- typeCheckBasicExpr operand
  case un_op of
    NotOp -> do
      unless (arg_ty == tbool) $ throwError ("`not` requires bool, got " <> show arg_ty)
      return tbool
typeCheckBasicExpr BinOpE{bin_op, lhs, rhs} = do
  ty_lhs <- typeCheckBasicExpr lhs
  ty_rhs <- typeCheckBasicExpr rhs
  case bin_op of
    AndOp -> do
      unless (ty_lhs == tbool && ty_rhs == tbool) $
        throwError ("`and` requires bools, got " <> show [ty_lhs, ty_rhs])
      return tbool
    LEqOp -> return tbool
    LtOp -> return tbool
    AddOp -> do
      unless (ty_lhs == ty_rhs) $
        throwError
          ("`+` requires same type args, got " <> show [ty_lhs, ty_rhs])
      return ty_lhs
    MulOp -> do
      unless (ty_lhs == ty_rhs) $
        throwError
          ("`*` requires same type args, got " <> show [ty_lhs, ty_rhs])
      return ty_lhs
    SubOp -> do
      unless (ty_lhs == ty_rhs) $
        throwError
          ("`-` requires same type args, got " <> show [ty_lhs, ty_rhs])
      return ty_lhs
    XorOp -> do
      unless (ty_lhs == ty_rhs) $
        throwError
          ("`^` requires same type args, got " <> show [ty_lhs, ty_rhs])
      return ty_lhs
    EqOp -> do
      unless (ty_lhs == ty_rhs) $
        throwError
          ("`==` requires same type args, got " <> show [ty_lhs, ty_rhs])
      return ty_lhs
typeCheckBasicExpr TernaryE{branch, lhs, rhs} = do
  ty_branch <- typeCheckBasicExpr branch
  unless (ty_branch == tbool) $
    throwError (printf "`ifte` requires bool to branch, got %s" (show ty_branch))

  ty_lhs <- typeCheckBasicExpr lhs
  ty_rhs <- typeCheckBasicExpr rhs
  unless (ty_lhs == ty_rhs) $
    throwError ("`ifte` requires same lhs and rhs type, got " <> show [ty_lhs, ty_rhs])

  return ty_lhs
typeCheckBasicExpr NAryE{op, operands} = do
  arg_tys <- mapM typeCheckBasicExpr operands
  case op of
    MultiOrOp -> do
      unless (all (== tbool) arg_tys) $
        throwError ("`Or` requires bools, got " <> show arg_tys)
      return tbool

-- Array operations
typeCheckBasicExpr IndexE{arr_expr} = do
  typeCheckBasicExpr arr_expr >>= \case
    Arr _ t -> return t
    ty -> throwError ("expected array for index op, got " <> show ty)
typeCheckBasicExpr DynIndexE{arr_expr, ix_expr} = do
  t <-
    typeCheckBasicExpr arr_expr >>= \case
      Arr _ t -> return t
      ty -> throwError ("expected array for index op, got " <> show ty)

  typeCheckBasicExpr ix_expr >>= \case
    Fin _ -> return ()
    ty -> throwError ("expected value for index, got " <> show ty)

  return t
typeCheckBasicExpr UpdateArrE{arr_expr, ix_expr, rhs} = do
  t <-
    typeCheckBasicExpr arr_expr >>= \case
      Arr _ t -> return t
      ty -> throwError ("expected array for index op, got " <> show ty)

  typeCheckBasicExpr ix_expr >>= \case
    Fin _ -> return ()
    ty -> throwError ("expected value for index, got " <> show ty)

  rhs_ty <- typeCheckBasicExpr rhs
  when (rhs_ty /= t) $ do
    throwError (printf "expected array value type %s, got %s" (show t) (show rhs_ty))

  typeCheckBasicExpr arr_expr
typeCheckBasicExpr ProjectE{tup_expr, tup_ix_val} = do
  ts <-
    typeCheckBasicExpr tup_expr >>= \case
      Tup ts -> return ts
      ty -> throwError ("expected array for index op, got " <> show ty)
  return $ ts !! tup_ix_val

-- ================================================================================
-- Typing inference for ProtoLang statements and programs
-- ================================================================================

lookupFunE ::
  ( MonadError String m
  , MonadReader r m
  , HasFunCtx r primT
  , sizeT ~ SizeType r
  ) =>
  Ident ->
  m (FunDef primT)
lookupFunE fname =
  view (_funCtx . Ctx.at fname)
    >>= maybeWithError (printf "cannot find function `%s`" fname)

-- | Environment for type checking
type TypingEnv ext = FunCtx ext

-- | The TypeChecker monad
type TypeChecker ext = ReaderT (TypingEnv ext) (StateT (TypingCtx (SizeType ext)) (Either String))

-- --------------------------------------------------------------------------------
-- Primitives
-- --------------------------------------------------------------------------------
class (TypeCheckable sizeT, sizeT ~ SizeType ext) => TypeCheckablePrimitive ext sizeT | ext -> sizeT where
  typeCheckPrimitive ::
    forall ext' m.
    ( m ~ TypeChecker ext'
    , sizeT ~ SizeType ext'
    ) =>
    ext ->
    m [VarType sizeT]
  default typeCheckPrimitive ::
    forall ext' m.
    ( Generic ext
    , GTypeCheckablePrimitive (Rep ext) sizeT
    , m ~ TypeChecker ext'
    , sizeT ~ SizeType ext'
    ) =>
    ext ->
    m [VarType sizeT]
  typeCheckPrimitive p = gtypeCheckPrimitive (from p)

instance (TypeCheckable sizeT) => TypeCheckablePrimitive (Core sizeT precT) sizeT where
  typeCheckPrimitive = \case {}

class (TypeCheckable sizeT) => GTypeCheckablePrimitive f sizeT | f -> sizeT where
  gtypeCheckPrimitive ::
    forall ext' m ext.
    ( m ~ TypeChecker ext'
    , sizeT ~ SizeType ext'
    ) =>
    f ext ->
    m [VarType sizeT]

instance (GTypeCheckablePrimitive p1 sizeT, GTypeCheckablePrimitive p2 sizeT) => GTypeCheckablePrimitive (p1 :+: p2) sizeT where
  gtypeCheckPrimitive (L1 p) = gtypeCheckPrimitive p
  gtypeCheckPrimitive (R1 p) = gtypeCheckPrimitive p

instance (GTypeCheckablePrimitive p sizeT) => GTypeCheckablePrimitive (M1 i c p) sizeT where
  gtypeCheckPrimitive (M1 x) = gtypeCheckPrimitive x

instance (TypeCheckablePrimitive p sizeT, sizeT ~ SizeType p) => GTypeCheckablePrimitive (K1 i p) sizeT where
  gtypeCheckPrimitive (K1 x) = typeCheckPrimitive x

-- --------------------------------------------------------------------------------
-- Core Language
-- --------------------------------------------------------------------------------

typeCheckDistrExpr ::
  forall primT sizeT.
  (TypeCheckable sizeT) =>
  DistrExpr sizeT ->
  TypeChecker primT [VarType sizeT]
typeCheckDistrExpr UniformE{sample_ty} = pure [sample_ty]
typeCheckDistrExpr BernoulliE{} = pure [tbool]

-- | Typecheck an expression and return the output types
typeCheckExpr ::
  forall primT sizeT.
  ( TypeCheckablePrimitive primT sizeT
  , sizeT ~ SizeType primT
  ) =>
  Expr primT ->
  TypeChecker primT [VarType sizeT]
typeCheckExpr BasicExprE{basic_expr} = do
  gamma <- use id
  lift $ do
    ty <- runReaderT ?? gamma $ typeCheckBasicExpr basic_expr
    return [ty]
typeCheckExpr RandomSampleE{distr_expr} = typeCheckDistrExpr distr_expr
-- f(x, ...)
typeCheckExpr FunCallE{fname, args} = do
  FunDef{param_types, ret_types} <- lookupFunE fname

  arg_tys <- mapM Ctx.lookup args
  unless (arg_tys == param_types) $
    throwError (fname <> " expects " <> show param_types <> ", got " <> show (zip args arg_tys))

  return ret_types

-- `primitive`
typeCheckExpr PrimCallE{prim} = typeCheckPrimitive prim
-- loop ...
typeCheckExpr LoopE{initial_args, loop_body_fun} =
  do
    -- extract the current context
    gamma <- use id
    in_tys <- runReaderT ?? gamma $ mapM Ctx.lookup' initial_args
    FunDef{param_types, ret_types} <- lookupFunE loop_body_fun
    unless (ret_types == in_tys) $
      throwError "Initial input argument types should match output types of the loop function."
    unless (init param_types == ret_types) $
      throwError "Initial N - 1 input param types should match output types of the loop function."
    when (null param_types) $
      throwError "There is should be at least one parameter types."
    case last param_types of
      (Fin _) -> return ret_types
      _ -> throwError "Last type of the loop function should be a Fin type."

{- | Typecheck a statement, given the current context and function definitions.
 If successful, the typing context is updated.
-}
typeCheckStmt ::
  forall primT sizeT.
  ( TypeCheckablePrimitive primT sizeT
  , sizeT ~ SizeType primT
  ) =>
  Stmt primT ->
  TypeChecker primT ()
-- single statement
typeCheckStmt ExprS{rets, expr} = do
  out_tys <- typeCheckExpr expr
  when (length out_tys /= length rets) $ do
    throwError $
      ("Expected " <> show (length out_tys) <> " outputs, but given " <> show (length out_tys) <> " vars to bind.")
        <> (" (return variables: " <> show rets <> ", output types: " <> show out_tys <> ")")
  zipWithM_ Ctx.put rets out_tys
-- sequence
typeCheckStmt (SeqS ss) = mapM_ typeCheckStmt ss
-- ifte
typeCheckStmt IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- Ctx.lookup cond
  unless (cond_ty == tbool) $
    throwError $
      "`if` condition must be a boolean, got " <> show (cond, cond_ty)

  sigma_t <- withSandbox $ typeCheckStmt s_true >> use id
  sigma_f <- withSandbox $ typeCheckStmt s_false >> use id
  when (sigma_t /= sigma_f) $
    throwError ("if: branches must declare same variables, got " <> show [sigma_t, sigma_f])
  id .= sigma_t

-- | Type check a single function.
typeCheckFun ::
  ( TypeCheckablePrimitive primT sizeT
  , sizeT ~ SizeType primT
  ) =>
  FunCtx primT ->
  FunDef primT ->
  Either String ()
typeCheckFun
  funCtx
  FunDef
    { param_types
    , ret_types
    , mbody = Just FunBody{param_names, ret_names, body_stmt}
    } = do
    when (length param_types /= length param_names) $
      throwError "number of parameters must match the types"
    when (length ret_types /= length ret_names) $
      throwError "number of returns must match the types"

    let gamma = Ctx.fromList $ zip param_names param_types
    gamma' <- execStateT ?? gamma $ runReaderT ?? funCtx $ typeCheckStmt body_stmt
    forM_ (zip ret_names ret_types) $ \(x, t) -> do
      when (has _Just (gamma ^. Ctx.at x)) $ do
        throwError $ printf "parameter `%s` cannot be returned, please copy it into a new variable and return that" x
      t' <- gamma' ^. Ctx.at x & maybe (throwError $ printf "missing in returns: %s" x) pure
      when (t /= t') $
        throwError $
          printf
            "return term %s: expected type %s, got type %s"
            x
            (show t)
            (show t')
typeCheckFun _ FunDef{mbody = Nothing} = return ()

-- | Type check a full program (i.e. list of functions).
typeCheckProg ::
  (TypeCheckablePrimitive primT sizeT) =>
  Program primT ->
  Either String ()
typeCheckProg (Program fs) =
  evalStateT ?? Ctx.empty $
    forM_ fs $ \NamedFunDef{fun_name, fun_def} -> do
      ctx <- use id
      lift $ typeCheckFun ctx fun_def
      Ctx.ins fun_name .= fun_def

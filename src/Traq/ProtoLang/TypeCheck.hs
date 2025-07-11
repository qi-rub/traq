{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.ProtoLang.TypeCheck (
  -- * Typing Context
  TypingCtx,
  HasTypingCtx (..),

  -- * Typeable size types
  TypeCheckable (..),

  -- * Types
  TypingEnv,
  TypeChecker,
  TypeCheckablePrimitive (..),

  -- * Checkers
  typeCheckBasicExpr,
  typeCheckStmt,
  typeCheckFun,
  typeCheckProg,
) where

import Control.Monad (forM_, unless, when, zipWithM_)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Trans (lift)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Typing Context
-- ================================================================================

-- | A context mapping variables to their types.
type TypingCtx sizeT = Ctx.Context (VarType sizeT)

type instance SizeType (TypingCtx sizeT) = sizeT

class HasTypingCtx p where
  _typingCtx :: (sizeT ~ SizeType p) => Lens' p (TypingCtx sizeT)

instance HasTypingCtx (TypingCtx sizeT) where _typingCtx = id

-- ================================================================================
-- Typecheckable size types
-- ================================================================================

-- | SizeT that can be type-checked. Needs to have a boolean type, and max of two types.
class (Eq sizeT, Show sizeT, Num sizeT) => TypeCheckable sizeT where
  tbool :: VarType sizeT
  tmax :: VarType sizeT -> VarType sizeT -> VarType sizeT
  irange :: sizeT -> Int

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)
  irange n = fromIntegral n

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)
  irange n = n

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
    AddOp -> return $ tmax ty_lhs ty_rhs
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

-- ================================================================================
-- Typing inference for ProtoLang statements and programs
-- ================================================================================

lookupFunE ::
  ( MonadError String m
  , MonadReader r m
  , HasFunCtx r
  , sizeT ~ SizeType r
  , primT ~ PrimitiveType r
  ) =>
  Ident ->
  m (FunDef primT sizeT)
lookupFunE fname =
  view (_funCtx . Ctx.at fname)
    >>= maybeWithError (printf "cannot find function `%s`" fname)

-- | Environment for type checking
type TypingEnv primT sizeT = FunCtx primT sizeT

-- | The TypeChecker monad
type TypeChecker primT sizeT = MyReaderStateT (TypingEnv primT sizeT) (TypingCtx sizeT) (Either String)

class TypeCheckablePrimitive primT sizeT where
  typeCheckPrimitive ::
    (TypeCheckable sizeT) =>
    -- | primitive
    primT ->
    -- | arguments
    [Ident] ->
    TypeChecker primsT sizeT [VarType sizeT]

instance TypeCheckablePrimitive Void sizeT where
  typeCheckPrimitive prim _ = absurd prim

-- | Typecheck an expression and return the output types
typeCheckExpr ::
  forall primT sizeT.
  ( TypeCheckablePrimitive primT sizeT
  , TypeCheckable sizeT
  ) =>
  Expr primT sizeT ->
  TypeChecker primT sizeT [VarType sizeT]
typeCheckExpr BasicExprE{basic_expr} = do
  gamma <- use id
  lift $ do
    ty <- runMyReaderT ?? gamma $ typeCheckBasicExpr basic_expr
    return [ty]
-- f(x, ...)
typeCheckExpr FunCallE{fun_kind = FunctionCall fun, args} = do
  FunDef{param_types, ret_types} <- lookupFunE fun

  arg_tys <- mapM Ctx.lookup args
  unless (arg_tys == param_types) $
    throwError (fun <> " expects " <> show param_types <> ", got " <> show (zip args arg_tys))

  return ret_types

-- `primitive`[...](...)
typeCheckExpr FunCallE{fun_kind = PrimitiveCall prim, args} = typeCheckPrimitive prim args

{- | Typecheck a statement, given the current context and function definitions.
 If successful, the typing context is updated.
-}
typeCheckStmt ::
  forall primT sizeT.
  ( TypeCheckablePrimitive primT sizeT
  , TypeCheckable sizeT
  ) =>
  Stmt primT sizeT ->
  TypeChecker primT sizeT ()
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
  , TypeCheckable sizeT
  ) =>
  FunCtx primT sizeT ->
  FunDef primT sizeT ->
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
    gamma' <- execMyReaderStateT (typeCheckStmt body_stmt) funCtx gamma
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
  ( TypeCheckablePrimitive primT sizeT
  , TypeCheckable sizeT
  ) =>
  TypingCtx sizeT ->
  Program primT sizeT ->
  Either String (TypingCtx sizeT)
typeCheckProg gamma Program{funCtx, stmt} = do
  mapM_ (typeCheckFun funCtx) funCtx
  execMyReaderStateT (typeCheckStmt stmt) funCtx gamma

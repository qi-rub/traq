module QCompose.ProtoLang.TypeCheck (
  -- * Types
  TypingEnv,
  TypingCtx,
  TypeCheckable (..),
  TypeChecker,
  TypeCheckablePrimitive (..),

  -- * Checkers
  checkPrimitive,
  checkExpr,
  checkStmt,
  typeCheckFun,
  typeCheckProg,
) where

import Control.Monad (forM_, unless, when, zipWithM_)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS (MonadReader)
import Text.Printf (printf)

import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx

import Data.Void (Void, absurd)
import QCompose.Control.Monad
import QCompose.Prelude
import QCompose.ProtoLang.Syntax

-- | SizeT that can be type-checked. Needs to have a boolean type, and max of two types.
class (Eq sizeT, Show sizeT, Num sizeT) => TypeCheckable sizeT where
  tbool :: VarType sizeT
  tmax :: VarType sizeT -> VarType sizeT -> VarType sizeT

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

-- | Environment for type checking
type TypingEnv primT sizeT = FunCtx primT sizeT

-- | A context mapping variables to their types.
type TypingCtx sizeT = Ctx.Context (VarType sizeT)

-- | The TypeChecker monad
type TypeChecker primT sizeT = MyReaderStateT (TypingEnv primT sizeT) (TypingCtx sizeT) (Either String)

lookupFunE ::
  (MonadReader (FunCtx primT sizeT) m, MonadError String m) =>
  Ident ->
  m (FunDef primT sizeT)
lookupFunE fname =
  view (Ctx.at fname)
    >>= maybeWithError (printf "cannot find function `%s`" fname)

class TypeCheckablePrimitive primT where
  typeCheckPrimitive ::
    (TypeCheckable sizeT) =>
    -- | primitive
    primT ->
    -- | arguments
    [Ident] ->
    TypeChecker primsT sizeT [VarType sizeT]

instance TypeCheckablePrimitive Void where
  typeCheckPrimitive prim _ = absurd prim

-- | Typecheck a subroutine call
{-# DEPRECATED checkPrimitive "Refactor Prims" #-}
checkPrimitive ::
  (TypeCheckable sizeT) =>
  -- | subroutine name
  Ident ->
  -- | subroutine params
  [Ident] ->
  -- | arguments
  [Ident] ->
  TypeChecker primT sizeT [VarType sizeT]
-- contains
checkPrimitive "any" [predicate] args = do
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_types, ret_types} <- lookupFunE predicate

  when (ret_types /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (init param_types /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool]

-- search
checkPrimitive "search" [predicate] args = do
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_types, ret_types} <- lookupFunE predicate

  when (ret_types /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (init param_types /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool, last param_types]

-- unsupported
checkPrimitive prim_name prim_params _ =
  throwError $ printf "unsupported subroutine %s[%s]" prim_name (show prim_params)

-- | Typecheck an expression and return the output types
checkExpr ::
  forall primT sizeT.
  (TypeCheckablePrimitive primT, TypeCheckable sizeT) =>
  Expr primT sizeT ->
  TypeChecker primT sizeT [VarType sizeT]
-- x
checkExpr VarE{arg} = do
  ty <- Ctx.lookup arg
  return [ty]
-- const v : t
checkExpr ConstE{ty} = return [ty]
-- `op` x
checkExpr UnOpE{un_op, arg} = do
  arg_ty <- Ctx.lookup arg
  ty <- case un_op of
    NotOp -> do
      unless (arg_ty == tbool) $ throwError ("`not` requires bool, got " <> show arg_ty)
      return tbool
  return [ty]
-- x `op` y
checkExpr BinOpE{bin_op, lhs, rhs} = do
  ty_lhs <- Ctx.lookup lhs
  ty_rhs <- Ctx.lookup rhs
  ty <- case bin_op of
    AndOp -> do
      unless (ty_lhs == tbool && ty_rhs == tbool) $
        throwError ("`and` requires bools, got " <> show [ty_lhs, ty_rhs])
      return tbool
    LEqOp -> return tbool
    AddOp -> return $ tmax ty_lhs ty_rhs
  return [ty]
-- ifte b x y
checkExpr TernaryE{branch, lhs, rhs} = do
  ty_branch <- Ctx.lookup branch
  unless (ty_branch == tbool) $
    throwError (printf "`ifte` requires bool to branch, got %s" (show ty_branch))

  ty_lhs <- Ctx.lookup lhs
  ty_rhs <- Ctx.lookup rhs
  unless (ty_lhs == ty_rhs) $
    throwError ("`ifte` requires same lhs and rhs type, got " <> show [ty_lhs, ty_rhs])

  return [ty_lhs]

-- f(x, ...)
checkExpr FunCallE{fun_kind = FunctionCall fun, args} = do
  FunDef{param_types, ret_types} <- lookupFunE fun

  arg_tys <- mapM Ctx.lookup args
  unless (arg_tys == param_types) $
    throwError (fun <> " expects " <> show param_types <> ", got " <> show (zip args arg_tys))

  return ret_types

-- `subroutine`(...)
checkExpr FunCallE{fun_kind = PrimitiveCallOld prim_name prim_params, args} = do
  checkPrimitive prim_name prim_params args
-- `primitive`[...](...)
checkExpr FunCallE{fun_kind = PrimitiveCall prim, args} = typeCheckPrimitive prim args

{- | Typecheck a statement, given the current context and function definitions.
 If successful, the typing context is updated.
-}
checkStmt ::
  forall primT sizeT.
  (TypeCheckablePrimitive primT, TypeCheckable sizeT) =>
  Stmt primT sizeT ->
  TypeChecker primT sizeT ()
-- single statement
checkStmt ExprS{rets, expr} = do
  out_tys <- checkExpr expr
  when (length out_tys /= length rets) $ do
    throwError $
      ("Expected " <> show (length out_tys) <> " outputs, but given " <> show (length out_tys) <> " vars to bind.")
        <> (" (return variables: " <> show rets <> ", output types: " <> show out_tys <> ")")
  zipWithM_ Ctx.put rets out_tys
-- sequence
checkStmt (SeqS ss) = mapM_ checkStmt ss
-- ifte
checkStmt IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- Ctx.lookup cond
  unless (cond_ty == tbool) $
    throwError $
      "`if` condition must be a boolean, got " <> show (cond, cond_ty)

  sigma_t <- withSandbox $ checkStmt s_true >> use id
  sigma_f <- withSandbox $ checkStmt s_false >> use id
  when (sigma_t /= sigma_f) $
    throwError ("if: branches must declare same variables, got " <> show [sigma_t, sigma_f])
  id .= sigma_t

-- | Type check a single function.
typeCheckFun :: (TypeCheckablePrimitive primT, TypeCheckable a) => FunCtx primT a -> FunDef primT a -> Either String ()
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
    gamma' <- execMyReaderStateT (checkStmt body_stmt) funCtx gamma
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
typeCheckProg :: (TypeCheckablePrimitive primT, TypeCheckable a) => TypingCtx a -> Program primT a -> Either String (TypingCtx a)
typeCheckProg gamma Program{funCtx, stmt} = do
  mapM_ (typeCheckFun funCtx) funCtx
  execMyReaderStateT (checkStmt stmt) funCtx gamma

module QCompose.ProtoLang.TypeCheck (
  TypingCtx,
  TypeCheckable (..),
  TypeChecker,
  checkPrimitive,
  checkExpr,
  checkStmt,
  typeCheckFun,
  typeCheckProg,
) where

import Control.Monad (forM_, unless, when, zipWithM_)
import Control.Monad.Except (throwError)
import Text.Printf (printf)

import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.Monad
import QCompose.Prelude
import QCompose.ProtoLang.Monad
import QCompose.ProtoLang.Syntax

lookupFunE :: Ident -> FunCtx a -> TypeChecker a (FunDef a)
lookupFunE fname funCtx =
  maybeWithError (printf "cannot find function `%s`" fname) $
    funCtx ^. Ctx.at fname

-- | Typecheck a subroutine call
checkPrimitive ::
  (TypeCheckable a) =>
  FunCtx a ->
  -- | subroutine name
  Ident ->
  -- | subroutine params
  [Ident] ->
  -- | arguments
  [Ident] ->
  TypeChecker a [VarType a]
-- contains
checkPrimitive funCtx "any" [predicate] args = do
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_types, ret_types} <- funCtx & lookupFunE predicate

  when (ret_types /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (init param_types /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool]

-- search
checkPrimitive funCtx "search" [predicate] args = do
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_types, ret_types} <- funCtx & lookupFunE predicate

  when (ret_types /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (init param_types /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool, last param_types]

-- unsupported
checkPrimitive _ prim_name prim_params _ =
  throwError $ printf "unsupported subroutine %s[%s]" prim_name (show prim_params)

-- | Typecheck an expression and return the output types
checkExpr ::
  forall a.
  (TypeCheckable a) =>
  FunCtx a ->
  Expr a ->
  TypeChecker a [VarType a]
-- x
checkExpr _ VarE{arg} = do
  ty <- Ctx.lookup arg
  return [ty]
-- const v : t
checkExpr _ ConstE{ty} = return [ty]
-- `op` x
checkExpr _ UnOpE{un_op, arg} = do
  arg_ty <- Ctx.lookup arg
  ty <- case un_op of
    NotOp -> do
      unless (arg_ty == tbool) $ throwError ("`not` requires bool, got " <> show arg_ty)
      return tbool
  return [ty]
-- x `op` y
checkExpr _ BinOpE{bin_op, lhs, rhs} = do
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
checkExpr _ TernaryE{branch, lhs, rhs} = do
  ty_branch <- Ctx.lookup branch
  unless (ty_branch == tbool) $
    throwError (printf "`ifte` requires bool to branch, got %s" (show ty_branch))

  ty_lhs <- Ctx.lookup lhs
  ty_rhs <- Ctx.lookup rhs
  unless (ty_lhs == ty_rhs) $
    throwError ("`ifte` requires same lhs and rhs type, got " <> show [ty_lhs, ty_rhs])

  return [ty_lhs]

-- f(x, ...)
checkExpr funCtx FunCallE{fun_kind = FunctionCall fun, args} = do
  FunDef{param_types, ret_types} <- funCtx & lookupFunE fun

  arg_tys <- mapM Ctx.lookup args
  unless (arg_tys == param_types) $
    throwError (fun <> " expects " <> show param_types <> ", got " <> show (zip args arg_tys))

  return ret_types

-- `subroutine`(...)
checkExpr funCtx FunCallE{fun_kind = PrimitiveCall prim_name prim_params, args} = do
  checkPrimitive funCtx prim_name prim_params args

{- | Typecheck a statement, given the current context and function definitions.
 If successful, the typing context is updated.
-}
checkStmt ::
  forall a.
  (TypeCheckable a) =>
  FunCtx a ->
  Stmt a ->
  TypeChecker a ()
-- single statement
checkStmt funCtx ExprS{rets, expr} = do
  out_tys <- checkExpr funCtx expr
  when (length out_tys /= length rets) $ do
    throwError $
      ("Expected " <> show (length out_tys) <> " outputs, but given " <> show (length out_tys) <> " vars to bind.")
        <> (" (return variables: " <> show rets <> ", output types: " <> show out_tys <> ")")
  zipWithM_ Ctx.put rets out_tys
-- sequence
checkStmt funCtx (SeqS ss) = mapM_ (checkStmt funCtx) ss
-- ifte
checkStmt funCtx IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- Ctx.lookup cond
  unless (cond_ty == tbool) $
    throwError $
      "`if` condition must be a boolean, got " <> show (cond, cond_ty)

  sigma_t <- withSandbox $ checkStmt funCtx s_true >> use id
  sigma_f <- withSandbox $ checkStmt funCtx s_false >> use id
  when (sigma_t /= sigma_f) $
    throwError ("if: branches must declare same variables, got " <> show [sigma_t, sigma_f])
  id .= sigma_t

-- | Type check a single function.
typeCheckFun :: (TypeCheckable a) => FunCtx a -> FunDef a -> Either String ()
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
    gamma' <- execMyStateT (checkStmt funCtx body_stmt) gamma
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
typeCheckProg :: (TypeCheckable a) => TypingCtx a -> Program a -> Either String (TypingCtx a)
typeCheckProg gamma Program{funCtx, stmt} = do
  mapM_ (typeCheckFun funCtx) funCtx
  execMyStateT (checkStmt funCtx stmt) gamma

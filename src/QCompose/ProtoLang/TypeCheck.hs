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
import Control.Monad.State (StateT, execStateT, get, put)
import Data.List (uncons)
import Lens.Micro
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.MonadHelpers
import QCompose.Prelude
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Printing

type TypingCtx a = Ctx.Context (VarType a)

class (Eq a, Show a) => TypeCheckable a where
  tbool :: VarType a
  tmax :: VarType a -> VarType a -> VarType a

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

-- | The TypeChecker monad
type TypeChecker a = StateT (TypingCtx a) (Either String)

lookupFunE :: Ident -> FunCtx a -> TypeChecker a (FunDef a)
lookupFunE fname funCtx =
  maybeWithError (printf "cannot find function `%s`" fname) $
    funCtx ^. to fun_defs . Ctx.at fname

-- | Typecheck a subroutine call
checkPrimitive ::
  (TypeCheckable a) =>
  FunCtx a ->
  -- | subroutine tag
  Primitive ->
  -- | arguments
  [Ident] ->
  TypeChecker a [VarType a]
-- contains
checkPrimitive funCtx Contains all_args = do
  let sub_name = "`" <> toCodeString Contains <> "`"

  (predicate, args) <- uncons all_args & maybe (throwError $ sub_name <> " needs 1 argument (predicate)") pure
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_binds = pred_params, ret_binds = pred_rets} <- funCtx & lookupFunE predicate

  when (map snd pred_rets /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (map snd (init pred_params) /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool]

-- search
checkPrimitive funCtx Search all_args = do
  let sub_name = toCodeString Search

  (predicate, args) <- uncons all_args & maybe (throwError $ sub_name <> " needs 1 argument (predicate)") pure
  arg_tys <- mapM Ctx.lookup args

  FunDef{param_binds = pred_params, ret_binds = pred_rets} <- funCtx & lookupFunE predicate

  when (map snd pred_rets /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (map snd (init pred_params) /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [tbool, snd $ last pred_params]

-- unsupported
-- checkPrimitive _ sub _ _ = throwError $ "unsupported subroutine " <> show sub

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
-- Oracle(x, ...)
checkExpr FunCtx{oracle_decl} FunCallE{fun_kind = OracleCall, args} = do
  let OracleDecl{param_types, ret_types} = oracle_decl

  arg_tys <- mapM Ctx.lookup args
  unless (arg_tys == param_types) $
    throwError ("oracle expects " <> show param_types <> ", got " <> show args)

  return ret_types

-- f(x, ...)
checkExpr funCtx FunCallE{fun_kind = FunctionCall fun, args} = do
  FunDef{param_binds, ret_binds} <- funCtx & lookupFunE fun

  arg_tys <- mapM Ctx.lookup args
  let param_tys = map snd param_binds
  unless (arg_tys == param_tys) $
    throwError (fun <> " expects " <> show param_tys <> ", got " <> show (zip args arg_tys))

  return $ map snd ret_binds

-- `subroutine`(...)
checkExpr funCtx FunCallE{fun_kind = PrimitiveCall sub, args} = do
  checkPrimitive funCtx sub args

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

  sigma_t <- withSandbox $ checkStmt funCtx s_true >> get
  sigma_f <- withSandbox $ checkStmt funCtx s_false >> get
  when (sigma_t /= sigma_f) $
    throwError ("if: branches must declare same variables, got " <> show [sigma_t, sigma_f])
  put sigma_t

-- | Type check a single function.
typeCheckFun :: (TypeCheckable a) => FunCtx a -> FunDef a -> Either String (TypingCtx a)
typeCheckFun funCtx FunDef{param_binds, ret_binds, body} = do
  let gamma = Ctx.fromList param_binds
  gamma' <- execStateT (checkStmt funCtx body) gamma
  forM_ ret_binds $ \(x, t) -> do
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

  return gamma'

-- | Type check a full program (i.e. list of functions).
typeCheckProg :: (TypeCheckable a) => TypingCtx a -> Program a -> Either String (TypingCtx a)
typeCheckProg gamma Program{funCtx = funCtx@FunCtx{fun_defs}, stmt} = do
  mapM_ (typeCheckFun funCtx) fun_defs
  execStateT (checkStmt funCtx stmt) gamma

{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.ProtoLang.TypeCheck (
  TypingCtx,
  TypeCheckable (..),
  checkSubroutine',
  checkStmt,
  typeCheckFun,
  typeCheckProg,
  isWellTyped,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (StateT, execStateT, get, lift, put)
import Data.Either (isRight)
import Data.List (uncons)
import qualified Data.Map as M
import Lens.Micro

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Context
import QCompose.Utils.Printing

type TypingCtx a = VarContext (VarType a)

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

lookupFunE :: Ident -> FunCtx a -> Either String (FunDef a)
lookupFunE fname funCtx =
  lookupFun fname funCtx `catchError` \_ -> throwError ("cannot find function " <> fname)

-- | Typecheck a subroutine call
checkSubroutine' ::
  (TypeCheckable a) =>
  FunCtx a ->
  -- | subroutine tag
  Subroutine ->
  -- | arguments
  [Ident] ->
  -- | returns
  [Ident] ->
  TypeChecker a [(Ident, VarType a)]
-- contains
checkSubroutine' funCtx Contains all_args rets = do
  let sub_name = "`" <> toCodeString Contains <> "`"

  ok <- case rets of
    [ok] -> return ok
    _ -> throwError $ sub_name <> " expects 1 argument, got " <> show rets

  (predicate, args) <- uncons all_args & maybe (throwError $ sub_name <> " needs 1 argument (predicate)") pure
  arg_tys <- mapM lookupVar args

  FunDef{param_binds = pred_params, ret_binds = pred_rets} <- lift $ funCtx & lookupFunE predicate

  when (map snd pred_rets /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (map snd (init pred_params) /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [(ok, tbool)]

-- search
checkSubroutine' funCtx Search all_args rets = do
  let sub_name = toCodeString Search

  (ok, sol) <- case rets of
    [ok, sol] -> return (ok, sol)
    _ -> throwError $ sub_name <> " expects 2 argument, got " <> show rets

  (predicate, args) <- uncons all_args & maybe (throwError $ sub_name <> " needs 1 argument (predicate)") pure
  arg_tys <- mapM lookupVar args

  FunDef{param_binds = pred_params, ret_binds = pred_rets} <- lift $ funCtx & lookupFunE predicate

  when (map snd pred_rets /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (map snd (init pred_params) /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [(ok, tbool), (sol, snd $ last pred_params)]

-- unsupported
-- checkSubroutine' _ sub _ _ = throwError $ "unsupported subroutine " <> show sub

{- | Typecheck a statement, given the current context and function definitions.
| If successful, the typing context is updated.
-}
checkStmt ::
  forall a.
  (TypeCheckable a) =>
  FunCtx a ->
  Stmt a ->
  TypeChecker a ()
checkStmt funCtx (SeqS ss) = mapM_ (checkStmt funCtx) ss
checkStmt funCtx IfThenElseS{cond, s_true, s_false} = do
  cond_ty <- lookupVar cond
  unless (cond_ty == tbool) $
    throwError $
      "`if` condition must be a boolean, got " <> show (cond, cond_ty)

  sigma <- get

  sigma_t <- lift $ execStateT (checkStmt funCtx s_true) sigma
  when (sigma M.\\ sigma_t /= M.empty) $
    error "Invalid final state, missing initial variables"
  let outs_t = sigma_t M.\\ sigma

  sigma_f <- lift $ execStateT (checkStmt funCtx s_false) sigma
  when (sigma M.\\ sigma_t /= M.empty) $
    error "Invalid final state, missing initial variables"
  let outs_f = sigma_f M.\\ sigma

  unless (outs_t == outs_f) $
    throwError ("if: branches must declare same variables, got " <> show [outs_t, outs_f])
  put sigma_t
checkStmt funCtx@FunCtx{oracle_decl} s = checkStmt' s >>= mapM_ (uncurry putValue)
  where
    -- Type check and return the new variable bindings.
    -- This does _not_ update the typing context!
    checkStmt' :: Stmt a -> TypeChecker a [(Ident, VarType a)]
    checkStmt' (SeqS _) = error "unreachable"
    checkStmt' IfThenElseS{} = error "unreachable"
    -- x <- x'
    checkStmt' AssignS{arg, ret} = do
      arg_ty <- lookupVar arg
      return [(ret, arg_ty)]

    -- x <- v : t
    checkStmt' ConstS{ret, ty} = do
      return [(ret, ty)]

    -- x <- op a
    checkStmt' UnOpS{ret, un_op, arg} = do
      ty <- lookupVar arg
      case un_op of
        NotOp -> unless (ty == tbool) $ throwError ("`not` requires bool, got " <> show ty)
      return [(ret, tbool)]

    -- x <- a op b
    checkStmt' BinOpS{ret, bin_op, lhs, rhs} = do
      ty_lhs <- lookupVar lhs
      ty_rhs <- lookupVar rhs
      case bin_op of
        AndOp -> unless (ty_lhs == tbool && ty_rhs == tbool) $ throwError ("`and` requires bools, got " <> show [ty_lhs, ty_rhs])
        _ -> return ()

      let t = case bin_op of
            AndOp -> tbool
            LEqOp -> tbool
            AddOp -> tmax ty_lhs ty_rhs
      return [(ret, t)]

    -- ret <- Oracle(args)
    checkStmt' FunCallS{fun_kind = OracleCall, rets, args} = do
      let OracleDecl{param_types, ret_types} = oracle_decl

      arg_tys <- mapM lookupVar args
      unless (arg_tys == param_types) $
        throwError ("oracle expects " <> show param_types <> ", got " <> show args)

      when (length rets /= length ret_types) $
        throwError ("oracle returns " <> show (length ret_types) <> " values, but RHS has " <> show rets)

      return $ zip rets ret_types

    -- ret <- f(args)
    checkStmt' FunCallS{fun_kind = FunctionCall fun, rets, args} = do
      FunDef _ fn_params fn_rets _ <- lift $ funCtx & lookupFunE fun

      arg_tys <- mapM lookupVar args
      let param_tys = map snd fn_params
      unless (arg_tys == param_tys) $
        throwError (fun <> " expects " <> show param_tys <> ", got " <> show (zip args arg_tys))

      when (length rets /= length fn_rets) $
        throwError ("oracle returns " <> show (length fn_rets) <> " values, but RHS has " <> show rets)

      return $ zip rets (map snd fn_rets)

    -- ok <- any(f, args)
    checkStmt' FunCallS{fun_kind = SubroutineCall sub, rets, args} = checkSubroutine' funCtx sub args rets

-- | Type check a single function.
typeCheckFun :: (TypeCheckable a) => FunCtx a -> FunDef a -> Either String (TypingCtx a)
typeCheckFun funCtx FunDef{param_binds, ret_binds, body} = do
  let gamma = M.fromList param_binds
  gamma' <- execStateT (checkStmt funCtx body) gamma
  forM_ ret_binds $ \(x, t) -> do
    let t' = gamma' M.! x
    when (t /= t') $
      throwError
        ( "return term "
            <> x
            <> ": expected type "
            <> show t
            <> ", got type "
            <> show t'
        )
  return gamma'

-- | Type check a full program (i.e. list of functions).
typeCheckProg :: (TypeCheckable a) => TypingCtx a -> Program a -> Either String (TypingCtx a)
typeCheckProg gamma Program{funCtx = funCtx@FunCtx{fun_defs}, stmt} = do
  mapM_ (typeCheckFun funCtx) fun_defs
  execStateT (checkStmt funCtx stmt) gamma

-- | Helper boolean predicate to check if a program is well-typed
isWellTyped :: (TypeCheckable a) => TypingCtx a -> Program a -> Bool
isWellTyped gamma = isRight . typeCheckProg gamma

{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.ProtoLang.TypeCheck where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, execStateT, get, lift, put)
import Data.Either (isRight)
import Data.Map ((!))
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Context
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Printing

type TypingCtx a = VarContext (VarType a)

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
  StateT (TypingCtx a) (Either String) [(Ident, VarType a)]
checkSubroutine' funCtx Contains args rets = do
  ok <- case rets of
    [ok] -> return ok
    _ -> throwError $ toCodeString Contains <> " expects 1 argument, got " <> show rets

  let predicate = head args
  arg_tys <- mapM lookupVar $ tail args

  FunDef{params = pred_params, rets = pred_rets} <- lookupFun funCtx predicate

  when (map snd pred_rets /= [tbool]) $
    throwError "predicate must return a single Bool"

  when (map snd (init pred_params) /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return [(ok, tbool)]
checkSubroutine' _ _ _ _ = error "TODO"

{- | Typecheck a statement, given the current context and function definitions.
| If successful, the typing context is updated.
-}
checkStmt :: forall a. (TypeCheckable a) => FunCtx a -> Stmt a -> StateT (TypingCtx a) (Either String) ()
checkStmt funCtx (SeqS ss) = mapM_ (checkStmt funCtx) ss
checkStmt funCtx IfThenElseS{..} = do
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
checkStmt funCtx@FunCtx{..} s = checkStmt' s >>= mapM_ (uncurry putValue)
  where
    -- Type check and return the new variable bindings.
    -- This does _not_ update the typing context!
    checkStmt' :: Stmt a -> StateT (TypingCtx a) (Either String) [(Ident, VarType a)]
    checkStmt' (SeqS _) = error "unreachable"
    checkStmt' IfThenElseS{} = error "unreachable"
    -- x <- x'
    checkStmt' AssignS{..} = do
      arg_ty <- lookupVar arg
      return [(ret, arg_ty)]

    -- x <- v : t
    checkStmt' ConstS{..} = do
      return [(ret, ty)]

    -- x <- op a
    checkStmt' UnOpS{..} = do
      ty <- lookupVar arg
      case un_op of
        NotOp -> unless (ty == tbool) $ throwError ("`not` requires bool, got " <> show ty)
      return [(ret, tbool)]

    -- x <- a op b
    checkStmt' BinOpS{..} = do
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
      let OracleDecl{paramTypes = o_arg_tys, retTypes = o_ret_tys} = oracle

      arg_tys <- mapM lookupVar args
      unless (arg_tys == o_arg_tys) $
        throwError ("oracle expects " <> show o_arg_tys <> ", got " <> show args)

      when (length rets /= length o_ret_tys) $
        throwError ("oracle returns " <> show (length o_ret_tys) <> " values, but RHS has " <> show rets)

      return $ zip rets o_ret_tys

    -- ret <- f(args)
    checkStmt' FunCallS{fun_kind = FunctionCall fun, rets, args} = do
      FunDef _ fn_params fn_rets _ <- lookupFun funCtx fun

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
typeCheckFun :: (TypeCheckable a) => FunCtx a -> FunDef a -> Either String ()
typeCheckFun funCtx FunDef{..} = do
  let gamma = M.fromList params
  gamma' <- execStateT (checkStmt funCtx body) gamma
  forM_ rets $ \(x, t) -> do
    let t' = gamma' ! x
    when (t /= t') $
      throwError
        ( "return term "
            <> x
            <> ": expected type "
            <> show t
            <> ", got type "
            <> show t'
        )

-- | Type check a full program (i.e. list of functions).
typeCheckProg :: (TypeCheckable a) => Program a -> Either String ()
typeCheckProg Program{funCtx = funCtx@FunCtx{..}, stmt} = do
  mapM_ (typeCheckFun funCtx) funDefs
  evalStateT (checkStmt funCtx stmt) M.empty

-- | Helper boolean predicate to check if a program is well-typed
isWellTyped :: (TypeCheckable a) => Program a -> Bool
isWellTyped = isRight . typeCheckProg

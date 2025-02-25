{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.ProtoLang.TypeCheck where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, execStateT, get)
import Control.Monad.Trans (lift)
import Data.Either (isRight)
import Data.Map ((!))
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Context
import QCompose.ProtoLang.Syntax

class (Eq a, Show a) => TypeCheckable a where
  tbool :: VarType a
  tmax :: VarType a -> VarType a -> VarType a

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

type TypingCtx a = VarContext (VarType a)

-- | Checks that the given inputs are present in the context, and the outputs are not.
checkInOuts :: forall a. (Show a) => [Ident] -> [Ident] -> StateT (TypingCtx a) (Either String) ()
checkInOuts ins outs = do
  gamma <- get
  lift $ checkPresent gamma ins
  lift $ checkNotPresent gamma outs
  where
    checkNotPresent :: TypingCtx a -> [Ident] -> Either String ()
    checkNotPresent gamma xs = forM_ xs $ \x -> do
      when (M.member x gamma) $ Left ("variable " <> x <> " already defined: " <> show gamma)

    checkPresent :: TypingCtx a -> [Ident] -> Either String ()
    checkPresent gamma xs = forM_ xs $ \x -> do
      unless (M.member x gamma) $ Left ("variable " <> x <> " not found: " <> show gamma)

{- | Typecheck a statement, given the current context and function definitions.
| If successful, return the updated typing context.
-}
checkStmt :: forall a. (TypeCheckable a) => FunCtx a -> Stmt a -> StateT (TypingCtx a) (Either String) ()
checkStmt funCtx (SeqS ss) = mapM_ (checkStmt funCtx) ss
checkStmt funCtx@FunCtx{..} s = checkStmt' s >>= mapM_ (uncurry putValue)
  where
    -- type check and return the new variable bindings.
    checkStmt' :: Stmt a -> StateT (TypingCtx a) (Either String) [(Ident, VarType a)]
    -- x <- x'
    checkStmt' AssignS{..} = do
      checkInOuts [arg] [ret]
      arg_ty <- lookupVar arg
      return [(ret, arg_ty)]

    -- x <- v : t
    checkStmt' ConstS{..} = do
      checkInOuts [] [ret]
      return [(ret, ty)]

    -- x <- op a
    checkStmt' UnOpS{..} = do
      checkInOuts [arg] [ret]
      ty <- lookupVar arg
      case un_op of
        NotOp -> unless (ty == tbool) $ throwError ("`not` requires bool, got " <> show ty)
      return [(ret, tbool)]

    -- x <- a op b
    checkStmt' BinOpS{..} = do
      checkInOuts [lhs, rhs] [ret]
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
    checkStmt' (OracleS ret args) = do
      checkInOuts args ret

      let OracleDecl{paramTypes = o_arg_tys, retTypes = o_ret_tys} = oracle

      arg_tys <- mapM lookupVar args
      unless (arg_tys == o_arg_tys) $
        throwError ("oracle expects " <> show o_arg_tys <> ", got " <> show args)

      when (length ret /= length o_ret_tys) $
        throwError ("oracle returns " <> show (length o_ret_tys) <> " values, but RHS has " <> show ret)

      return $ zip ret o_ret_tys

    -- ret <- f(args)
    checkStmt' FunCallS{..} = do
      checkInOuts args rets

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx fun

      arg_tys <- mapM lookupVar args
      let param_tys = map snd fn_params
      unless (arg_tys == param_tys) $
        throwError (fun <> " expects " <> show param_tys <> ", got " <> show (zip args arg_tys))

      when (length rets /= length fn_rets) $
        throwError ("oracle returns " <> show (length fn_rets) <> " values, but RHS has " <> show rets)

      return $ zip rets (map snd fn_rets)

    -- x, ok <- search[f](args)
    checkStmt' SearchS{..} = do
      checkInOuts args [sol, ok]

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx predicate

      let fn_ret_tys = map snd fn_rets
      unless (fn_ret_tys == [tbool]) $
        throwError ("predicate " <> predicate <> " should return bool, got " <> show fn_rets)

      arg_tys <- mapM lookupVar args
      let param_tys = map snd fn_params
      unless (arg_tys == init param_tys) $
        throwError ("predicate " <> predicate <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, tbool), (sol, last param_tys)]

    -- ok <- contains[f](args)
    checkStmt' (ContainsS ok f args) = do
      checkInOuts args [ok]

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx f

      let fn_ret_tys = map snd fn_rets
      unless (fn_ret_tys == [tbool]) $
        throwError ("predicate " <> f <> " should return bool, got " <> show fn_rets)

      arg_tys <- mapM lookupVar args
      let param_tys = map snd fn_params
      unless (arg_tys == init param_tys) $
        throwError ("predicate " <> f <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, tbool)]

    -- if b then s_t else s_f
    checkStmt' (IfThenElseS b s_t s_f) = do
      checkInOuts [b] []
      outs_t <- checkStmt' s_t
      outs_f <- checkStmt' s_f
      unless (outs_t == outs_f) $ throwError ("if: branches must declare same variables, got " <> show [outs_t, outs_f])
      return outs_t

    -- s_1 ; s_2
    checkStmt' _ = error "unreachable"

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

{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.ProtoLang.TypeCheck where

import Control.Monad (foldM, forM_, unless, when)
import Data.Either (isRight)
import Data.Map ((!))
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

class TypeCheckable a where
  tbool :: VarType a
  tmax :: VarType a -> VarType a -> VarType a

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

type TypingCtx a = M.Map Ident (VarType a)

-- | Checks that the given inputs are present in the context, and the outputs are not.
checkInOuts :: Show a => TypingCtx a -> [Ident] -> [Ident] -> Either String ()
checkInOuts gamma ins outs = checkPresent ins >> checkNotPresent outs
  where
    checkNotPresent :: [Ident] -> Either String ()
    checkNotPresent xs = forM_ xs $ \x -> do
      when (M.member x gamma) $ Left ("variable " <> x <> " already defined: " <> show gamma)

    checkPresent :: [Ident] -> Either String ()
    checkPresent xs = forM_ xs $ \x -> do
      unless (M.member x gamma) $ Left ("variable " <> x <> " not found: " <> show gamma)

{- | Typecheck a statement, given the current context and function definitions.
| If successful, return the updated typing context.
-}
checkStmt :: forall a. (Eq a, Show a, TypeCheckable a) => FunCtx a -> Stmt a -> TypingCtx a -> Either String (TypingCtx a)
checkStmt funCtx (SSeq ss) gamma = foldM (flip $ checkStmt funCtx) gamma ss
checkStmt funCtx@FunCtx{..} s gamma = M.union gamma . M.fromList <$> checkStmt' s
  where
    -- type check and return the new variable bindings.
    checkStmt' :: Stmt a -> Either String [(Ident, VarType a)]
    -- x <- x'
    checkStmt' SAssign{..} = do
      checkInOuts gamma [arg] [ret]
      return [(ret, gamma ! arg)]

    -- x <- v : t
    checkStmt' SConst{..} = do
      checkInOuts gamma [] [ret]
      return [(ret, ty)]

    -- x <- op a
    checkStmt' SUnOp{..} = do
      checkInOuts gamma [arg] [ret]
      let ty = gamma ! arg
      case un_op of
        PNot -> unless (ty == tbool) $ Left ("`not` requires bool, got " <> show ty)
      return [(ret, tbool)]

    -- x <- a op b
    checkStmt' SBinOp{..} = do
      checkInOuts gamma [lhs, rhs] [ret]
      let ty_lhs = gamma ! lhs
      let ty_rhs = gamma ! rhs
      case bin_op of
        PAnd -> unless (ty_lhs == tbool && ty_rhs == tbool) $ Left ("`and` requires bools, got " <> show [ty_lhs, ty_rhs])
        _ -> return ()

      let t = case bin_op of
            PAnd -> tbool
            PLeq -> tbool
            PAdd -> tmax ty_lhs ty_rhs
      return [(ret, t)]

    -- ret <- Oracle(args)
    checkStmt' (SOracle ret args) = do
      checkInOuts gamma args ret

      let OracleDef{paramTypes = o_arg_tys, retTypes = o_ret_tys} = oracle

      let arg_tys = map (gamma !) args
      unless (arg_tys == o_arg_tys) $
        Left ("oracle expects " <> show o_arg_tys <> ", got " <> show args)

      when (length ret /= length o_ret_tys) $
        Left ("oracle returns " <> show (length o_ret_tys) <> " values, but RHS has " <> show ret)

      return $ zip ret o_ret_tys

    -- ret <- f(args)
    checkStmt' SFunCall{..} = do
      checkInOuts gamma args rets

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx fun

      let arg_tys = map (gamma !) args
      let param_tys = map snd fn_params
      unless (arg_tys == param_tys) $
        Left (fun <> " expects " <> show param_tys <> ", got " <> show (zip args arg_tys))

      when (length rets /= length fn_rets) $
        Left ("oracle returns " <> show (length fn_rets) <> " values, but RHS has " <> show rets)

      return $ zip rets (map snd fn_rets)

    -- x, ok <- search[f](args)
    checkStmt' SSearch{..} = do
      checkInOuts gamma args [sol, ok]

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx predicate

      let fn_ret_tys = map snd fn_rets
      unless (fn_ret_tys == [tbool]) $
        Left ("predicate " <> predicate <> " should return bool, got " <> show fn_rets)

      let arg_tys = map (gamma !) args
      let param_tys = map snd fn_params
      unless (arg_tys == init param_tys) $
        Left ("predicate " <> predicate <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, tbool), (sol, last param_tys)]

    -- ok <- contains[f](args)
    checkStmt' (SContains ok f args) = do
      checkInOuts gamma args [ok]

      FunDef _ fn_params fn_rets _ <- lookupFun funCtx f

      let fn_ret_tys = map snd fn_rets
      unless (fn_ret_tys == [tbool]) $
        Left ("predicate " <> f <> " should return bool, got " <> show fn_rets)

      let arg_tys = map (gamma !) args
      let param_tys = map snd fn_params
      unless (arg_tys == init param_tys) $
        Left ("predicate " <> f <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, tbool)]

    -- if b then s_t else s_f
    checkStmt' (SIfTE b s_t s_f) = do
      checkInOuts gamma [b] []
      outs_t <- checkStmt' s_t
      outs_f <- checkStmt' s_f
      unless (outs_t == outs_f) $ Left ("if: branches must declare same variables, got " <> show [outs_t, outs_f])
      return outs_t

    -- s_1 ; s_2
    checkStmt' _ = error "unreachable"

-- | Type check a single function.
typeCheckFun :: (Eq a, Show a, TypeCheckable a) => FunCtx a -> FunDef a -> Either String ()
typeCheckFun funCtx FunDef{..} = do
  let gamma = M.fromList params
  gamma' <- checkStmt funCtx body gamma
  forM_ rets $ \(x, t) ->
    do
      let t' = gamma' ! x
      when (t /= t') $
        Left
          ( "return term "
              <> x
              <> ": expected type "
              <> show t
              <> ", got type "
              <> show t'
          )

-- | Type check a full program (i.e. list of functions).
typeCheckProg :: (Eq a, Show a, TypeCheckable a) => Program a -> Either String ()
typeCheckProg Program{funCtx = funCtx@FunCtx{..}, stmt} = do
  mapM_ (typeCheckFun funCtx) funDefs
  _ <- checkStmt funCtx stmt M.empty
  return ()

-- | Helper boolean predicate to check if a program is well-typed
isWellTyped :: (Eq a, Show a, TypeCheckable a) => Program a -> Bool
isWellTyped = isRight . typeCheckProg

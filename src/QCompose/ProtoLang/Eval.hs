{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.ProtoLang.Eval (
  -- * Evaluations
  evalExpr,
  execStmt,
  evalFun,
  runProgram,

  -- * Values
  range,
  ProgramState,
  boolToValue,

  -- * Types and Monad
  FunInterp,
  FunInterpCtx,
  ExecutionEnv,
  ExecutionState,
  Evaluator,
  Executor,
  EvaluatablePrimitive (..),
) where

import Control.Monad (zipWithM_)
import Control.Monad.RWS (MonadState, runRWST)
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import Data.Void (Void, absurd)
import QCompose.Prelude
import QCompose.ProtoLang.Syntax

-- | The deterministic state of the program
type ProgramState = Ctx.Context Value

-- | Inject runtime data into a program
type FunInterp = [Value] -> [Value]

-- | A mapping of data injections
type FunInterpCtx = Ctx.Context FunInterp

-- | Environment for evaluation
type ExecutionEnv primsT sizeT = (FunCtx primsT sizeT, FunInterpCtx)

type ExecutionState sizeT = ProgramState

-- | Non-deterministic Execution Monad (i.e. no state)
type Evaluator primsT sizeT = MyReaderT (ExecutionEnv primsT sizeT) Tree.Tree

-- | Non-deterministic Execution Monad
type Executor primsT sizeT = MyReaderStateT (ExecutionEnv primsT sizeT) (ExecutionState sizeT) Tree.Tree

{- | Primitives that support evaluation:
 Can evaluate @primT@ under a context of @primsT@.
-}
class EvaluatablePrimitive primsT primT where
  evalPrimitive :: primT -> [Value] -> Evaluator primsT SizeT [Value]

instance EvaluatablePrimitive primsT Void where
  evalPrimitive = absurd

-- evaluation
range :: (Integral sizeT) => VarType sizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

lookupS :: (MonadState ProgramState m) => Ident -> m Value
lookupS = Ctx.unsafeLookup

putS :: (MonadState ProgramState m) => Ident -> Value -> m ()
putS = Ctx.unsafePut

boolToValue :: Bool -> Value
boolToValue True = 1
boolToValue False = 0

valueToBool :: Value -> Bool
valueToBool 0 = False
valueToBool _ = True

evalUnOp :: UnOp -> Value -> Value
evalUnOp NotOp 0 = 1
evalUnOp NotOp _ = 0

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp AddOp x y = x + y
evalBinOp LEqOp x y
  | x <= y = 1
  | otherwise = 0
evalBinOp AndOp 0 _ = 0
evalBinOp AndOp _ 0 = 0
evalBinOp AndOp _ _ = 1

evalExpr ::
  forall primsT.
  (EvaluatablePrimitive primsT primsT) =>
  Expr primsT SizeT ->
  Executor primsT SizeT [Value]
-- basic expressions
evalExpr VarE{arg} = do
  v <- lookupS arg
  return [v]
evalExpr ConstE{val} = return [val]
evalExpr UnOpE{un_op, arg} = do
  arg_val <- lookupS arg
  let ret_val = evalUnOp un_op arg_val
  return [ret_val]
evalExpr BinOpE{bin_op, lhs, rhs} = do
  lhs_val <- lookupS lhs
  rhs_val <- lookupS rhs
  let ret_val = evalBinOp bin_op lhs_val rhs_val
  return [ret_val]
evalExpr TernaryE{branch, lhs, rhs} = do
  b <- lookupS branch
  v <- lookupS $ if valueToBool b then lhs else rhs
  return [v]
-- function calls
evalExpr FunCallE{fun_kind = FunctionCall fun, args} = do
  arg_vals <- mapM lookupS args
  funCtx <- view _1
  let fun_def = funCtx ^. Ctx.at fun . singular _Just
  embedReaderT $ evalFun arg_vals fun_def

-- subroutines
evalExpr FunCallE{fun_kind = PrimitiveCall prim, args} = do
  vals <- mapM lookupS args
  embedReaderT $ evalPrimitive prim vals

execStmt :: (EvaluatablePrimitive primsT primsT) => Stmt primsT SizeT -> Executor primsT SizeT ()
execStmt ExprS{rets, expr} = do
  vals <- withSandbox $ evalExpr expr
  zipWithM_ putS rets vals
execStmt IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupS cond
  let s = if cond_val == 0 then s_false else s_true
  execStmt s
execStmt (SeqS ss) = mapM_ execStmt ss

evalFun :: (EvaluatablePrimitive primsT primsT) => [Value] -> FunDef primsT SizeT -> Evaluator primsT SizeT [Value]
evalFun vals_in FunDef{mbody = Just FunBody{param_names, ret_names, body_stmt}} =
  let params = Ctx.fromList $ zip param_names vals_in
   in withInjectedState params $ do
        execStmt body_stmt
        mapM lookupS ret_names
evalFun vals_in FunDef{fun_name, mbody = Nothing} = do
  interp <- view $ _2 . Ctx.at fun_name . singular _Just
  return $ interp vals_in

runProgram :: (EvaluatablePrimitive primsT primsT) => Program primsT SizeT -> FunInterpCtx -> ProgramState -> Tree.Tree ProgramState
runProgram Program{funCtx, stmt} oracleF st = view _2 <$> runRWST (execStmt stmt) env st
 where
  env = (funCtx, oracleF)

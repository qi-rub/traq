module QCompose.ProtoLang.Eval (
  -- * Evaluations
  execStmt,
  evalFun,
  runProgram,

  -- * Values
  range,
  ProgramState,
  boolToValue,
) where

import Control.Monad (filterM, zipWithM_)
import Control.Monad.Extra (anyM)
import Control.Monad.RWS (MonadState, runRWST)
import Control.Monad.Trans (lift)
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import QCompose.Prelude
import QCompose.ProtoLang.Monad
import QCompose.ProtoLang.Syntax

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

lookupS :: (MonadState ProgramState m) => Ident -> m Value
lookupS = Ctx.unsafeLookup

putS :: (MonadState ProgramState m) => Ident -> Value -> m ()
putS = Ctx.unsafePut

boolToValue :: Bool -> Value
boolToValue True = 1
boolToValue False = 0

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

evalExpr :: Expr SizeT -> Executor [Value]
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
-- function calls
evalExpr FunCallE{fun_kind = FunctionCall fun, args} = do
  arg_vals <- mapM lookupS args
  funCtx <- view _1
  let fun_def = funCtx ^. Ctx.at fun . singular _Just
  embedReaderT $ evalFun arg_vals fun_def

-- subroutines
-- `any` / `search`
evalExpr FunCallE{fun_kind = PrimitiveCall prim_name [predicate], args}
  | prim_name == "any" = do
      search_range <- get_search_range
      has_sol <- anyM evalPredicate search_range

      return [boolToValue has_sol]
  | prim_name == "search" = do
      search_range <- get_search_range
      sols <- filterM evalPredicate search_range

      let has_sol = not $ null sols
      let out_vals = if has_sol then sols else search_range
      lift $ Tree.choice [pure [boolToValue has_sol, v] | v <- out_vals]
 where
  get_search_range :: Executor [Value]
  get_search_range = do
    FunDef{param_types = pred_param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let s_arg_ty = last pred_param_types
    let search_range = range s_arg_ty
    return search_range

  evalPredicate :: Value -> Executor Bool
  evalPredicate val = (/= 0) <$> runPredicate "_search_arg" val

  runPredicate :: Ident -> Value -> Executor Value
  runPredicate s_arg val = withSandbox $ do
    Ctx.ins s_arg .= val
    head
      <$> evalExpr
        FunCallE{fun_kind = FunctionCall predicate, args = args ++ [s_arg]}
-- unsupported
evalExpr _ = error "unsupported"

execStmt :: Stmt SizeT -> Executor ()
execStmt ExprS{rets, expr} = do
  vals <- withSandbox $ evalExpr expr
  zipWithM_ putS rets vals
execStmt IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupS cond
  let s = if cond_val == 0 then s_false else s_true
  execStmt s
execStmt (SeqS ss) = mapM_ execStmt ss

evalFun :: [Value] -> FunDef SizeT -> Evaluator [Value]
evalFun vals_in FunDef{mbody = Just FunBody{param_names, ret_names, body_stmt}} =
  let params = Ctx.fromList $ zip param_names vals_in
   in withInjectedState params $ do
        execStmt body_stmt
        mapM lookupS ret_names
evalFun vals_in FunDef{fun_name, mbody = Nothing} = do
  interp <- view $ _2 . Ctx.at fun_name . singular _Just
  return $ interp vals_in

runProgram :: Program SizeT -> FunInterpCtx -> ProgramState -> Tree.Tree ProgramState
runProgram Program{funCtx, stmt} oracleF st = view _2 <$> runRWST (execStmt stmt) env st
 where
  env = (funCtx, oracleF)

module QCompose.ProtoLang.Eval (
  range,
  ProgramState,
  OracleInterp,
  evalStmt,
  evalFun,
  evalProgram,
) where

import Control.Monad (filterM, zipWithM_)
import Control.Monad.Extra (anyM)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Context
import QCompose.Utils.Tree

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

-- | The deterministic state of the program
type ProgramState = VarContext Value

-- | Type for a function that implements the oracle
type OracleInterp = [Value] -> [Value]

-- | Non-deterministic Evaluation Monad
type Evaluator = StateT ProgramState Tree

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

evalStmt :: FunCtx SizeT -> OracleInterp -> Stmt SizeT -> Evaluator ()
-- basic statements
evalStmt _ _ AssignS{ret, arg} = do
  at ret <~ use (at arg)
evalStmt _ _ ConstS{ret, val} = do
  at ret ?= val
evalStmt _ _ UnOpS{ret, un_op, arg} = do
  arg_val <- lookupVar arg
  at ret ?= evalUnOp un_op arg_val
evalStmt _ _ BinOpS{ret, bin_op, lhs, rhs} = do
  lhs_val <- lookupVar lhs
  rhs_val <- lookupVar rhs
  at ret ?= evalBinOp bin_op lhs_val rhs_val

-- compound statements
evalStmt funCtx oracleF IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupVar cond
  let s = if cond_val == 0 then s_false else s_true
  evalStmt funCtx oracleF s
evalStmt funCtx oracleF (SeqS ss) = mapM_ (evalStmt funCtx oracleF) ss
-- function calls
evalStmt _ oracleF FunCallS{fun_kind = OracleCall, args, rets} = do
  arg_vals <- mapM lookupVar args
  let ret_vals = oracleF arg_vals
  zipWithM_ putValue rets ret_vals
evalStmt funCtx oracleF FunCallS{fun_kind = FunctionCall fun, args, rets} = do
  arg_vals <- mapM lookupVar args
  fun_def <- lift $ funCtx & lookupFun fun
  ret_vals <- lift $ evalFun funCtx oracleF arg_vals fun_def
  zipWithM_ putValue rets ret_vals

-- subroutines
-- `any` / `search`
evalStmt funCtx oracleF FunCallS{fun_kind = SubroutineCall sub, rets = (ok : rets), args = (predicate : args)}
  | sub == Contains = do
      vals <- getSearchRange
      sigma <- get
      has_sol <- lift $ anyM (evalPredicate sigma) vals

      at ok ?= boolToValue has_sol
  | sub == Search = do
      let [sol] = rets

      vals <- getSearchRange
      sigma <- get
      sols <- lift $ filterM (evalPredicate sigma) vals

      let has_sol = not $ null sols
      let out_vals = if has_sol then sols else vals
      binds <- lift $ choice [pure [(ok, boolToValue has_sol), (sol, v)] | v <- out_vals]
      mapM_ (uncurry putValue) binds
  where
    getSearchRange = do
      FunDef{param_binds = pred_param_binds} <- lift $ funCtx & lookupFun predicate
      let (_, s_arg_ty) = last pred_param_binds
      return $ range s_arg_ty

    evalPredicate :: ProgramState -> Value -> Tree Bool
    evalPredicate sigma val = evalStateT (runPredicate "_search_arg" val) sigma

    runPredicate :: Ident -> Value -> Evaluator Bool
    runPredicate s_arg val = do
      putValue s_arg val
      _ <-
        evalProgram
          Program{funCtx, stmt = FunCallS{fun_kind = FunctionCall predicate, rets = [ok], args = args ++ [s_arg]}}
          oracleF
      val_ok <- lookupVar ok
      return $ val_ok /= 0

-- fallback
evalStmt _ _ _ = error "unsupported"

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> Tree [Value]
evalFun funCtx oracleF vals_in FunDef{param_binds, ret_binds, body} =
  (`evalStateT` params) $ do
    evalStmt funCtx oracleF body
    mapM lookupVar ret_names
  where
    param_names = map fst param_binds
    params = M.fromList $ zip param_names vals_in
    ret_names = map fst ret_binds

evalProgram :: Program SizeT -> OracleInterp -> Evaluator ()
evalProgram Program{funCtx, stmt} oracleF = evalStmt funCtx oracleF stmt

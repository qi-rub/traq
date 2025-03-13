module QCompose.ProtoLang.Eval (
  range,
  ProgramState,
  OracleInterp,
  evalStmt,
  evalFun,
  evalProgram,
) where

import Control.Monad.Except (catchError)
import Control.Monad.Extra (anyM)
import Control.Monad.State
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Context

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

type ProgramState = VarContext Value

type OracleInterp = [Value] -> [Value]

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

evalStmt :: FunCtx SizeT -> OracleInterp -> Stmt SizeT -> StateT ProgramState (Either String) [(Ident, Value)]
-- basic statements
evalStmt _ _ AssignS{ret, arg} = do
  arg_val <- lookupVar arg
  return [(ret, arg_val)]
evalStmt _ _ ConstS{ret, val} = do
  return [(ret, val)]
evalStmt _ _ UnOpS{ret, un_op, arg} = do
  arg_val <- lookupVar arg
  let ret_val = evalUnOp un_op arg_val
  return [(ret, ret_val)]
evalStmt _ _ BinOpS{ret, bin_op, lhs, rhs} = do
  lhs_val <- lookupVar lhs
  rhs_val <- lookupVar rhs
  let ret_val = evalBinOp bin_op lhs_val rhs_val
  return [(ret, ret_val)]

-- compound statements
evalStmt funCtx oracleF IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupVar cond
  let s = if cond_val == 0 then s_false else s_true
  evalStmt funCtx oracleF s
evalStmt funCtx oracleF (SeqS ss) = do
  mapM_ (\s -> evalProgram Program{funCtx, stmt = s} oracleF) ss
  return []

-- function calls
evalStmt _ oracleF FunCallS{fun_kind = OracleCall, args, rets} = do
  arg_vals <- mapM lookupVar args
  let ret_vals = oracleF arg_vals
  return $ zip rets ret_vals
evalStmt funCtx oracleF FunCallS{fun_kind = FunctionCall fun, args, rets} = do
  arg_vals <- mapM lookupVar args
  fun_def <- lookupFun funCtx fun
  ret_vals <- lift $ evalFun funCtx oracleF arg_vals fun_def
  return $ zip rets ret_vals

-- subroutines
-- `any`
evalStmt funCtx oracleF FunCallS{fun_kind = SubroutineCall Contains, rets, args = all_args} = do
  pred_fun_def <- lookupFun funCtx predicate
  let (_, s_arg_ty) = last $ params pred_fun_def
  sigma <- get
  has_sol <- lift $ anyM (evalPredicate sigma) (range s_arg_ty)
  return [(ok, if has_sol then 1 else 0)]
  where
    predicate = head all_args
    args = tail all_args
    [ok] = rets

    evalPredicate :: ProgramState -> Value -> (Either String) Bool
    evalPredicate sigma val = evalStateT (runPredicate "_search_arg" val) sigma

    runPredicate :: Ident -> Value -> StateT ProgramState (Either String) Bool
    runPredicate s_arg val = do
      putValue s_arg val
      _ <-
        evalProgram
          Program{funCtx, stmt = FunCallS{fun_kind = FunctionCall predicate, rets = [ok], args = args ++ [s_arg]}}
          oracleF
      val_ok <- lookupVar ok
      return $ val_ok /= 0
-- `search`
evalStmt _ _ FunCallS{fun_kind = SubroutineCall Search} = do
  error "non-deterministic"

evalProgram :: Program SizeT -> OracleInterp -> StateT ProgramState (Either String) [(Ident, Value)]
evalProgram Program{funCtx, stmt} oracleF = do
  rets <- evalStmt funCtx oracleF stmt `catchError` handleError
  mapM_ (uncurry putValue) rets
  return rets
  where
    handleError e = do
      st <- get
      lift $
        Left $
          unlines
            [ "Error running: " <> show stmt
            , "    with state: " <> show st
            , e
            ]

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> Either String [Value]
evalFun funCtx oracleF vals_in FunDef{params, rets, body} = (`evalStateT` M.empty) $ do
  zipWithM_ putValue param_names vals_in
  _ <- evalProgram Program{funCtx, stmt = body} oracleF
  mapM lookupVar ret_names
  where
    param_names = map fst params
    ret_names = map fst rets

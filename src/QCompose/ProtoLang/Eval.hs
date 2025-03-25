module QCompose.ProtoLang.Eval (
  range,
  ProgramState,
  OracleInterp,
  execStmt,
  evalFun,
  execProgram,
) where

import Control.Monad (filterM, zipWithM_)
import Control.Monad.Extra (anyM)
import Control.Monad.Reader (ReaderT, local)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Lens.Micro

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree
import QCompose.Utils.MonadHelpers

import QCompose.Prelude
import QCompose.ProtoLang.Syntax

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

-- | The deterministic state of the program
type ProgramState = Ctx.Context Value

-- | Type for a function that implements the oracle
type OracleInterp = [Value] -> [Value]

-- | Non-deterministic Evaluation Monad
type Evaluator = ReaderT ProgramState Tree.Tree

-- | Non-deterministic Execution Monad
type Executor = StateT ProgramState Tree.Tree

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

evalExpr :: FunCtx SizeT -> OracleInterp -> Expr SizeT -> Evaluator [Value]
-- basic expressions
evalExpr _ _ VarE{arg} = do
  v <- Ctx.lookup' arg
  return [v]
evalExpr _ _ ConstE{val} = return [val]
evalExpr _ _ UnOpE{un_op, arg} = do
  arg_val <- Ctx.lookup' arg
  let ret_val = evalUnOp un_op arg_val
  return [ret_val]
evalExpr _ _ BinOpE{bin_op, lhs, rhs} = do
  lhs_val <- Ctx.lookup' lhs
  rhs_val <- Ctx.lookup' rhs
  let ret_val = evalBinOp bin_op lhs_val rhs_val
  return [ret_val]
-- function calls
evalExpr _ oracleF FunCallE{fun_kind = OracleCall, args} = do
  arg_vals <- mapM Ctx.lookup' args
  return $ oracleF arg_vals
evalExpr funCtx oracleF FunCallE{fun_kind = FunctionCall fun, args} = do
  arg_vals <- mapM Ctx.lookup' args
  fun_def <- lift $ funCtx & lookupFun fun
  lift $ evalFun funCtx oracleF arg_vals fun_def

-- subroutines
-- `any` / `search`
evalExpr funCtx oracleF FunCallE{fun_kind = PrimitiveCall sub, args = (predicate : args)}
  | sub == Contains = do
      vals <- getSearchRange
      has_sol <- anyM evalPredicate vals

      return [boolToValue has_sol]
  | sub == Search = do
      vals <- getSearchRange
      sols <- filterM evalPredicate vals

      let has_sol = not $ null sols
      let out_vals = if has_sol then sols else vals
      lift $ Tree.choice [pure [boolToValue has_sol, v] | v <- out_vals]
 where
  getSearchRange = do
    FunDef{param_binds = pred_param_binds} <- lift $ funCtx & lookupFun predicate
    let (_, s_arg_ty) = last pred_param_binds
    return $ range s_arg_ty

  evalPredicate :: Value -> Evaluator Bool
  evalPredicate val = (/= 0) <$> runPredicate "_search_arg" val

  runPredicate :: Ident -> Value -> Evaluator Value
  runPredicate s_arg val = local (Ctx.at s_arg ?~ val) $ do
    head
      <$> evalExpr
        funCtx
        oracleF
        FunCallE{fun_kind = FunctionCall predicate, args = args ++ [s_arg]}
-- unsupported
evalExpr _ _ _ = error "unsupported"

execStmt :: FunCtx SizeT -> OracleInterp -> Stmt SizeT -> Executor ()
execStmt funCtx oracleF ExprS{rets, expr} = do
  vals <- withFrozenState $ evalExpr funCtx oracleF expr
  zipWithM_ Ctx.put rets vals
execStmt funCtx oracleF IfThenElseS{cond, s_true, s_false} = do
  cond_val <- Ctx.lookup cond
  let s = if cond_val == 0 then s_false else s_true
  execStmt funCtx oracleF s
execStmt funCtx oracleF (SeqS ss) = mapM_ (execStmt funCtx oracleF) ss

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> Tree.Tree [Value]
evalFun funCtx oracleF vals_in FunDef{param_binds, ret_binds, body} =
  (`evalStateT` params) $ do
    execStmt funCtx oracleF body
    mapM Ctx.lookup ret_names
 where
  param_names = map fst param_binds
  params = Ctx.fromList $ zip param_names vals_in
  ret_names = map fst ret_binds

execProgram :: Program SizeT -> OracleInterp -> Executor ()
execProgram Program{funCtx, stmt} oracleF = execStmt funCtx oracleF stmt

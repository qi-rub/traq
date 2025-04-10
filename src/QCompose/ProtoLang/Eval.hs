module QCompose.ProtoLang.Eval (
  -- * Evaluations
  execStmt,
  evalFun,
  runProgram,

  -- * Values
  range,
  ProgramState,
  OracleInterp,
  boolToValue,
) where

import Control.Monad (filterM, zipWithM_)
import Control.Monad.Extra (anyM)
import Control.Monad.RWS (MonadState, runRWST)
import Control.Monad.Reader (local)
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

lookupEnv :: Ident -> Evaluator Value
lookupEnv k = view $ _3 . Ctx.at k . singular _Just

lookupS :: (MonadState ProgramState m) => Ident -> m Value
lookupS k = use $ Ctx.at k . singular _Just

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

evalExpr :: Expr SizeT -> Evaluator [Value]
-- basic expressions
evalExpr VarE{arg} = do
  v <- lookupEnv arg
  return [v]
evalExpr ConstE{val} = return [val]
evalExpr UnOpE{un_op, arg} = do
  arg_val <- lookupEnv arg
  let ret_val = evalUnOp un_op arg_val
  return [ret_val]
evalExpr BinOpE{bin_op, lhs, rhs} = do
  lhs_val <- lookupEnv lhs
  rhs_val <- lookupEnv rhs
  let ret_val = evalBinOp bin_op lhs_val rhs_val
  return [ret_val]
-- function calls
evalExpr FunCallE{fun_kind = OracleCall, args} = do
  arg_vals <- mapM lookupEnv args
  oracleF <- view _2
  return $ oracleF arg_vals
evalExpr FunCallE{fun_kind = FunctionCall fun, args} = do
  arg_vals <- mapM lookupEnv args
  funCtx <- view _1
  oracleF <- view _2
  let fun_def = funCtx ^. to fun_defs . Ctx.at fun . singular _Just
  lift $ evalFun funCtx oracleF arg_vals fun_def

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
  get_search_range :: Evaluator [Value]
  get_search_range = do
    FunDef{param_binds = pred_param_binds} <- view $ _1 . to fun_defs . Ctx.at predicate . singular _Just
    let (_, s_arg_ty) = last pred_param_binds
    let search_range = range s_arg_ty
    return search_range

  evalPredicate :: Value -> Evaluator Bool
  evalPredicate val = (/= 0) <$> runPredicate "_search_arg" val

  runPredicate :: Ident -> Value -> Evaluator Value
  runPredicate s_arg val = local (_3 . Ctx.ins s_arg .~ val) $ do
    head
      <$> evalExpr
        FunCallE{fun_kind = FunctionCall predicate, args = args ++ [s_arg]}
-- unsupported
evalExpr _ = error "unsupported"

-- | Convert a frozen evaluation to an execution.
evalToExec :: Evaluator a -> Executor a
evalToExec m = do
  fns <- view _1
  o <- view _2
  sigma <- use id
  lift $ runMyReaderT m (fns, o, sigma)

execStmt :: Stmt SizeT -> Executor ()
execStmt ExprS{rets, expr} = do
  vals <- evalToExec $ evalExpr expr
  zipWithM_ putS rets vals
execStmt IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupS cond
  let s = if cond_val == 0 then s_false else s_true
  execStmt s
execStmt (SeqS ss) = mapM_ execStmt ss

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> Tree.Tree [Value]
evalFun funCtx oracleF vals_in FunDef{param_binds, ret_binds, body} =
  fmap (view _1) . (\m -> runRWST m env st) $ do
    execStmt body
    mapM lookupS ret_names
 where
  param_names = map fst param_binds
  params = Ctx.fromList $ zip param_names vals_in
  ret_names = map fst ret_binds

  env = (funCtx, oracleF)
  st = params

runProgram :: Program SizeT -> OracleInterp -> ProgramState -> Tree.Tree ProgramState
runProgram Program{funCtx, stmt} oracleF st = view _2 <$> runRWST (execStmt stmt) env st
 where
  env = (funCtx, oracleF)

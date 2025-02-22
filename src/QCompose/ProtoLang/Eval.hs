module QCompose.ProtoLang.Eval where

import Control.Monad.Except (throwError)
import Control.Monad.Extra (anyM)
import Control.Monad.State
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

type ProgramState = M.Map Ident Value

lookupVar :: Ident -> StateT ProgramState (Either String) Value
lookupVar x = do
  v <- gets (M.lookup x)
  lift $ maybe (throwError $ "cannot find variable " <> x) Right v

putValue :: Ident -> Value -> StateT ProgramState (Either String) ()
putValue x v = modify (M.insert x v)

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
evalStmt _ _ AssignS{..} = do
  arg_val <- lookupVar arg
  return [(ret, arg_val)]
evalStmt _ _ ConstS{..} = do
  return [(ret, val)]
evalStmt _ _ UnOpS{..} = do
  arg_val <- lookupVar arg
  let ret_val = evalUnOp un_op arg_val
  return [(ret, ret_val)]
evalStmt _ _ BinOpS{..} = do
  lhs_val <- lookupVar lhs
  rhs_val <- lookupVar rhs
  let ret_val = evalBinOp bin_op lhs_val rhs_val
  return [(ret, ret_val)]
evalStmt _ oracleF OracleS{..} = do
  arg_vals <- mapM lookupVar args
  let ret_vals = oracleF arg_vals
  return $ zip rets ret_vals
evalStmt funCtx oracleF IfThenElseS{..} = do
  cond_val <- lookupVar cond
  let s = if cond_val == 0 then s_false else s_true
  evalStmt funCtx oracleF s
evalStmt funCtx oracleF FunCallS{..} = do
  arg_vals <- mapM lookupVar args
  fun_def <- lookupFun funCtx fun
  ret_vals <- lift $ evalFun funCtx oracleF arg_vals fun_def
  return $ zip rets ret_vals
evalStmt funCtx oracleF ContainsS{..} = do
  pred_fun_def <- lookupFun funCtx predicate
  let (s_arg, s_arg_ty) = last $ params pred_fun_def
  sigma <- get
  has_sol <- lift $ anyM (evalPredicate sigma s_arg) (range s_arg_ty)
  return [(ok, if has_sol then 1 else 0)]
  where
    evalPredicate :: ProgramState -> Ident -> Value -> (Either String) Bool
    evalPredicate sigma s_arg val = evalStateT (runPredicate s_arg val) sigma

    runPredicate :: Ident -> Value -> StateT ProgramState (Either String) Bool
    runPredicate s_arg val = do
      putValue s_arg val
      _ <-
        evalProgram
          Program{funCtx, stmt = FunCallS{rets = [ok], fun = predicate, args = args ++ [s_arg]}}
          oracleF
      val_ok <- lookupVar ok
      return $ val_ok /= 0
evalStmt _ _ SearchS{} = error "non-deterministic"
evalStmt funCtx oracleF (SeqS ss) =
  concat
    <$> mapM
      (\s -> evalProgram Program{funCtx, stmt = s} oracleF)
      ss

evalProgram :: Program SizeT -> OracleInterp -> StateT ProgramState (Either String) [(Ident, Value)]
evalProgram Program{funCtx, stmt} oracleF = do
  rets <- evalStmt funCtx oracleF stmt
  mapM_ (uncurry putValue) rets
  return rets

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> Either String [Value]
evalFun funCtx oracleF vals_in FunDef{..} = (`evalStateT` M.empty) $ do
  zipWithM_ putValue param_names vals_in
  _ <- evalProgram Program{funCtx, stmt = body} oracleF
  mapM lookupVar ret_names
  where
    param_names = map fst params
    ret_names = map fst rets

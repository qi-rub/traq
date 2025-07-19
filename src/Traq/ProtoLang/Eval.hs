{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.ProtoLang.Eval (
  -- * Evaluating Basic Expressions
  ProgramState,
  HasProgramState (..),
  valueToBool,
  evalBasicExpr,

  -- * Evaluations
  evalExpr,
  execStmt,
  evalFun,
  runProgram,

  -- * Values
  range,
  boolToValue,
  validateValueType,

  -- * Types and Monad
  EvaluatablePrimitive (..),

  -- ** Evaluation
  FunInterp,
  FunInterpCtx,
  HasFunInterpCtx (..),
  EvaluationEnv,
  HasEvaluationEnv (..),
  Evaluator,

  -- ** Execution (state updating)
  ExecutionState,
  Executor,
) where

import Control.Monad (zipWithM_)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, execStateT)
import Control.Monad.Trans (lift)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Tree as Tree

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Evaluating Basic Expressions
-- ================================================================================

-- | Check if a given runtime value is of a given type.
validateValueType :: forall sizeT. (sizeT ~ SizeT) => VarType sizeT -> Value sizeT -> Bool
validateValueType (Fin n) (FinV v) = 0 <= v && v < n

boolToValue :: Bool -> Value SizeT
boolToValue True = FinV 1
boolToValue False = FinV 0

valueToBool :: Value SizeT -> Bool
valueToBool (FinV 0) = False
valueToBool (FinV _) = True

evalUnOp :: (sizeT ~ SizeT) => UnOp -> Value sizeT -> Value sizeT
evalUnOp NotOp = boolToValue . not . valueToBool

evalBinOp :: (sizeT ~ SizeT) => BinOp -> Value sizeT -> Value sizeT -> Value sizeT
evalBinOp AddOp (FinV x) (FinV y) = FinV $ x + y
evalBinOp LEqOp (FinV x) (FinV y)
  | x <= y = boolToValue True
  | otherwise = boolToValue False
evalBinOp AndOp v1 v2 = boolToValue $ valueToBool v1 && valueToBool v2
evalBinOp _ _ _ = error "invalid inputs"

evalOp :: (sizeT ~ SizeT) => NAryOp -> [Value sizeT] -> Value sizeT
evalOp MultiOrOp = boolToValue . any valueToBool

-- | The deterministic state of the program
type ProgramState sizeT = Ctx.Context (Value sizeT)

class HasProgramState p where
  _state :: (sizeT ~ SizeType p) => Lens' p (ProgramState sizeT)

instance HasProgramState (ProgramState sizeT) where _state = id

evalBasicExpr ::
  ( MonadReader env m
  , HasProgramState env
  , sizeT ~ SizeType env
  , sizeT ~ SizeT
  ) =>
  BasicExpr sizeT ->
  m (Value sizeT)
evalBasicExpr VarE{var} = view $ _state . Ctx.at var . singular _Just
evalBasicExpr ConstE{val} = return val
evalBasicExpr UnOpE{un_op, operand} = do
  arg_val <- evalBasicExpr operand
  return $ evalUnOp un_op arg_val
evalBasicExpr BinOpE{bin_op, lhs, rhs} = do
  lhs_val <- evalBasicExpr lhs
  rhs_val <- evalBasicExpr rhs
  return $ evalBinOp bin_op lhs_val rhs_val
evalBasicExpr TernaryE{branch, lhs, rhs} = do
  b <- evalBasicExpr branch
  evalBasicExpr $ if valueToBool b then lhs else rhs
evalBasicExpr NAryE{op, operands} = do
  vals <- mapM evalBasicExpr operands
  return $ evalOp op vals
evalBasicExpr ParamE{} = error "unsupported: parameters"

-- ================================================================================
-- Evaluating ProtoLang Programs
-- ================================================================================

-- | Inject runtime data into a program
type FunInterp sizeT = [Value sizeT] -> [Value sizeT]

type instance SizeType (FunInterp sizeT) = sizeT

-- | A mapping of data injections
type FunInterpCtx sizeT = Ctx.Context (FunInterp sizeT)

class HasFunInterpCtx p where
  _funInterpCtx :: (sizeT ~ SizeType p) => Lens' p (FunInterpCtx sizeT)

instance HasFunInterpCtx (FunInterpCtx sizeT) where _funInterpCtx = id

-- | Environment for evaluation
data EvaluationEnv primsT sizeT = EvaluationEnv (FunCtx primsT sizeT) (FunInterpCtx sizeT)

type instance SizeType (EvaluationEnv primsT sizeT) = sizeT
type instance PrimitiveType (EvaluationEnv primsT sizeT) = primsT

class HasEvaluationEnv p where
  _evaluationEnv ::
    (primsT ~ PrimitiveType p, sizeT ~ SizeType p) =>
    Lens' p (EvaluationEnv primsT sizeT)

instance HasEvaluationEnv (EvaluationEnv primsT sizeT) where _evaluationEnv = id

instance HasDefault (EvaluationEnv primsT sizeT) where default_ = EvaluationEnv default_ default_

instance HasFunCtx (EvaluationEnv primsT sizeT) where
  _funCtx focus (EvaluationEnv f fi) = focus f <&> \f' -> EvaluationEnv f' fi

instance HasFunInterpCtx (EvaluationEnv primsT sizeT) where
  _funInterpCtx focus (EvaluationEnv f fi) = focus fi <&> EvaluationEnv f

type ExecutionState sizeT = ProgramState sizeT

-- | Non-deterministic Execution Monad (i.e. no state)
type Evaluator primsT sizeT = ReaderT (EvaluationEnv primsT sizeT) Tree.Tree

-- | Non-deterministic Execution Monad
type Executor primsT sizeT = StateT (ExecutionState sizeT) (Evaluator primsT sizeT)

{- | Primitives that support evaluation:
 Can evaluate @primT@ under a context of @primsT@.
-}
class EvaluatablePrimitive primsT primT where
  evalPrimitive :: (sizeT ~ SizeT) => primT -> [Value sizeT] -> Evaluator primsT sizeT [Value sizeT]

instance EvaluatablePrimitive primsT Void where
  evalPrimitive = absurd

-- evaluation
range :: (Integral sizeT) => VarType sizeT -> [Value sizeT]
range (Fin n) = FinV <$> [0 .. n - 1]

lookupS ::
  forall sizeT env m.
  ( MonadState env m
  , HasProgramState env
  , sizeT ~ SizeType env
  ) =>
  Ident ->
  m (Value sizeT)
lookupS x = use $ _state . Ctx.at x . singular _Just

putS ::
  ( MonadState env m
  , HasProgramState env
  , sizeT ~ SizeType env
  ) =>
  Ident ->
  Value sizeT ->
  m ()
putS x v = _state . Ctx.ins x .= v

evalExpr ::
  forall primsT m.
  ( EvaluatablePrimitive primsT primsT
  , m ~ Evaluator primsT SizeT
  ) =>
  Expr primsT SizeT ->
  ProgramState SizeT ->
  m [Value SizeT]
evalExpr BasicExprE{basic_expr} sigma = do
  val <- runReaderT ?? sigma $ evalBasicExpr basic_expr
  return [val]

-- function calls
evalExpr FunCallE{fun_kind = FunctionCall fun, args} sigma = do
  arg_vals <- evalStateT ?? sigma $ mapM lookupS args
  fun_def <- view $ _funCtx . Ctx.at fun . singular _Just
  evalFun arg_vals fun fun_def

-- subroutines
evalExpr FunCallE{fun_kind = PrimitiveCall prim, args} sigma = do
  vals <- evalStateT ?? sigma $ mapM lookupS args
  evalPrimitive prim vals

execStmt ::
  forall primsT m.
  ( EvaluatablePrimitive primsT primsT
  , m ~ Executor primsT SizeT
  ) =>
  Stmt primsT SizeT ->
  m ()
execStmt ExprS{rets, expr} = do
  sigma <- use _state
  vals <- lift $ evalExpr expr sigma
  zipWithM_ putS rets vals
execStmt IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupS cond
  let s = if valueToBool cond_val then s_true else s_false
  execStmt s
execStmt (SeqS ss) = mapM_ execStmt ss

evalFun ::
  forall primsT m.
  ( EvaluatablePrimitive primsT primsT
  , m ~ Evaluator primsT SizeT
  ) =>
  -- | arguments
  [Value SizeT] ->
  -- | function name
  Ident ->
  -- | function
  FunDef primsT SizeT ->
  m [Value SizeT]
evalFun vals_in _ FunDef{mbody = Just FunBody{param_names, ret_names, body_stmt}} =
  let params = Ctx.fromList $ zip param_names vals_in
   in (evalStateT ?? params) $ do
        execStmt body_stmt
        mapM lookupS ret_names
evalFun vals_in fun_name FunDef{mbody = Nothing} = do
  fn_interp <- view $ _funInterpCtx . Ctx.at fun_name . singular _Just
  return $ fn_interp vals_in

runProgram ::
  forall primsT.
  (EvaluatablePrimitive primsT primsT) =>
  Program primsT SizeT ->
  FunInterpCtx SizeT ->
  ProgramState SizeT ->
  Tree.Tree (ProgramState SizeT)
runProgram Program{funCtx, stmt} funInterpCtx st =
  execStmt stmt
    & (execStateT ?? st)
    & (runReaderT ?? env)
 where
  env =
    default_
      & (_funCtx .~ funCtx)
      & (_funInterpCtx .~ funInterpCtx)

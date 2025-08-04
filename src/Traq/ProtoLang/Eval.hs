{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.ProtoLang.Eval (
  -- * Evaluating Basic Expressions
  ProgramState,
  HasProgramState (..),
  evalBasicExpr,

  -- * Evaluations
  evalExpr,
  execStmt,
  evalFun,
  runProgram,

  -- * Values
  defaultV,
  validateValueType,
  CoerceValue (..),
  toValue,
  fromValue,
  valueToBool,
  domain,

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

import Control.Monad (replicateM, zipWithM_)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, execStateT)
import Control.Monad.Trans (lift)
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Values
-- ================================================================================

-- | default value of a given type
defaultV :: forall sizeT. (sizeT ~ SizeT) => VarType sizeT -> Value sizeT
defaultV (Fin _) = FinV 0
defaultV (Arr n t) = ArrV $ replicate n (defaultV t)
defaultV (Tup ts) = TupV $ map defaultV ts

-- | Check if a given runtime value is of a given type.
validateValueType :: forall sizeT. (sizeT ~ SizeT) => VarType sizeT -> Value sizeT -> Bool
validateValueType (Fin n) (FinV v) = 0 <= v && v < n
validateValueType (Arr n t) (ArrV vs) = length vs == n && all (validateValueType t) vs
validateValueType (Tup ts) (TupV vs) = length vs == length ts && and (zipWith validateValueType ts vs)
validateValueType _ _ = False

-- | Coerce a haskell value into our value type.
class CoerceValue t where
  -- | safely convert a runtime value to @Value@.
  safeToValue :: t -> Maybe (Value SizeT)

  -- | safely convert a @Value@ to a runtime value.
  safeFromValue :: Value SizeT -> Maybe t

instance CoerceValue Bool where
  safeToValue True = pure $ FinV 1
  safeToValue False = pure $ FinV 0

  safeFromValue (FinV 0) = pure False
  safeFromValue (FinV _) = pure True
  safeFromValue _ = fail "cannot convert non-Fin to Bool"

instance (CoerceValue a) => CoerceValue [a] where
  safeToValue xs = ArrV <$> traverse safeToValue xs

  safeFromValue (ArrV xs) = traverse safeFromValue xs
  safeFromValue _ = fail "cannot convert non-array value to [a]"

-- | Convert an input into @Value@
toValue :: (CoerceValue t) => t -> Value SizeT
toValue = fromJust . safeToValue

-- | Convert a @Value@ to the given type.
fromValue :: (CoerceValue t) => Value SizeT -> t
fromValue = fromJust . safeFromValue

valueToBool :: Value SizeT -> Bool
valueToBool = fromValue

-- | Set of all values of a given type
domain :: (Integral sizeT) => VarType sizeT -> [Value sizeT]
domain (Fin n) = FinV <$> [0 .. n - 1]
domain (Arr n t) = ArrV <$> replicateM (fromIntegral n) (domain t)
domain (Tup ts) = TupV <$> traverse domain ts

-- ================================================================================
-- Evaluating Basic Expressions
-- ================================================================================

evalUnOp :: (sizeT ~ SizeT) => UnOp -> Value sizeT -> Value sizeT
evalUnOp NotOp = toValue . not . fromValue

evalBinOp :: (sizeT ~ SizeT) => BinOp -> Value sizeT -> Value sizeT -> Value sizeT
evalBinOp AddOp (FinV x) (FinV y) = FinV $ x + y
evalBinOp LEqOp (FinV x) (FinV y)
  | x <= y = toValue True
  | otherwise = toValue False
evalBinOp AndOp v1 v2 = toValue $ fromValue v1 && fromValue v2
evalBinOp _ _ _ = error "invalid inputs"

evalOp :: (sizeT ~ SizeT) => NAryOp -> [Value sizeT] -> Value sizeT
evalOp MultiOrOp = toValue . any valueToBool

-- | @elemOfArr i a@ returns a[i]
elemOfArr :: (sizeT ~ SizeT) => Value sizeT -> Value sizeT -> Value sizeT
elemOfArr (FinV i) (ArrV xs) = xs !! i
elemOfArr _ _ = error "invalid inputs"

-- | @modifyArr a i v@ sets a[i] to v.
modifyArr :: (sizeT ~ SizeT) => Value sizeT -> Value sizeT -> Value sizeT -> Value sizeT
modifyArr (ArrV xs) (FinV i) v = ArrV $ xs & ix i .~ v
modifyArr _ _ _ = error "invalid inputs"

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
  evalBasicExpr $ if fromValue b then lhs else rhs
evalBasicExpr NAryE{op, operands} = do
  vals <- mapM evalBasicExpr operands
  return $ evalOp op vals
evalBasicExpr ParamE{} = error "unsupported: parameters"
evalBasicExpr IndexE{arr_expr, ix_val} = do
  arr <- evalBasicExpr arr_expr
  return $ elemOfArr (FinV ix_val) arr
evalBasicExpr DynIndexE{arr_expr, ix_expr} = do
  arr <- evalBasicExpr arr_expr
  i <- evalBasicExpr ix_expr
  return $ elemOfArr i arr
evalBasicExpr UpdateArrE{arr_expr, ix_expr, rhs} = do
  arr <- evalBasicExpr arr_expr
  i <- evalBasicExpr ix_expr
  v <- evalBasicExpr rhs
  return $ modifyArr arr i v
evalBasicExpr ProjectE{tup_expr, ix_val} = do
  arr <- evalBasicExpr tup_expr
  return $ arr ^?! _TupV . ix ix_val

-- ================================================================================
-- Program Evaluation Context
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

-- ================================================================================
-- Program Execution
-- ================================================================================

type ExecutionState sizeT = ProgramState sizeT

-- | Non-deterministic Execution Monad (i.e. no state)
type Evaluator primsT sizeT costT = ReaderT (EvaluationEnv primsT sizeT) (Prob.Prob costT)

-- | Non-deterministic Execution Monad
type Executor primsT sizeT costT = StateT (ExecutionState sizeT) (Evaluator primsT sizeT costT)

{- | Primitives that support evaluation:
 Can evaluate @primT@ under a context of @primsT@.
-}
class (Fractional costT) => EvaluatablePrimitive primsT primT costT where
  evalPrimitive :: (sizeT ~ SizeT) => primT -> [Value sizeT] -> Evaluator primsT sizeT costT [Value sizeT]

instance (Fractional costT) => EvaluatablePrimitive primsT Void costT where
  evalPrimitive = absurd

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
  forall primsT costT m.
  ( EvaluatablePrimitive primsT primsT costT
  , m ~ Evaluator primsT SizeT costT
  ) =>
  Expr primsT SizeT ->
  ProgramState SizeT ->
  m [Value SizeT]
evalExpr BasicExprE{basic_expr} sigma = do
  val <- runReaderT ?? sigma $ evalBasicExpr basic_expr
  return [val]

-- probabilistic instructions
evalExpr UniformRandomE{sample_ty} _ = error "TODO"
evalExpr BiasedCoinE{prob_one} _ = error "TODO"
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
  forall primsT costT m.
  ( EvaluatablePrimitive primsT primsT costT
  , m ~ Executor primsT SizeT costT
  ) =>
  Stmt primsT SizeT ->
  m ()
execStmt ExprS{rets, expr} = do
  sigma <- use _state
  vals <- lift $ evalExpr expr sigma
  zipWithM_ putS rets vals
execStmt IfThenElseS{cond, s_true, s_false} = do
  cond_val <- lookupS cond
  let s = if fromValue cond_val then s_true else s_false
  execStmt s
execStmt (SeqS ss) = mapM_ execStmt ss

evalFun ::
  forall primsT costT m.
  ( EvaluatablePrimitive primsT primsT costT
  , m ~ Evaluator primsT SizeT costT
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
  forall primsT costT.
  (EvaluatablePrimitive primsT primsT costT) =>
  Program primsT SizeT ->
  FunInterpCtx SizeT ->
  ProgramState SizeT ->
  Prob.Distr costT (ProgramState SizeT)
runProgram Program{funCtx, stmt} funInterpCtx st =
  execStmt stmt
    & (execStateT ?? st)
    & (runReaderT ?? env)
    & Prob.runProb
 where
  env =
    default_
      & (_funCtx .~ funCtx)
      & (_funInterpCtx .~ funInterpCtx)

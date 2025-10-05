{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
  domainSize,
  domain,

  -- * Types and Monad
  EvaluationMonad,
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

import Control.Monad (foldM, replicateM, when, zipWithM_)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Bits (Bits (xor))
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)
import GHC.Generics
import Text.Printf (printf)

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

-- | Size of the value set of a given type.
domainSize :: (Num sizeT) => VarType sizeT -> sizeT
domainSize (Fin n) = n
domainSize (Arr n t) = n * domainSize t
domainSize (Tup ts) = product $ map domainSize ts

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
evalBinOp MulOp (FinV x) (FinV y) = FinV $ x * y
evalBinOp SubOp (FinV x) (FinV y) = FinV $ x - y
evalBinOp XorOp (FinV x) (FinV y) = FinV (xor x y)
evalBinOp LEqOp (FinV x) (FinV y) = toValue $ x <= y
evalBinOp LtOp (FinV x) (FinV y) = toValue $ x < y
evalBinOp AndOp v1 v2 = toValue $ fromValue v1 && fromValue v2
evalBinOp op lhs rhs = error $ printf "evalBinOp failed: %s (%s) (%s)" (show op) (show lhs) (show rhs)

evalOp :: (sizeT ~ SizeT) => NAryOp -> [Value sizeT] -> Value sizeT
evalOp MultiOrOp = toValue . any valueToBool

-- | @elemOfArr i a@ returns a[i]
elemOfArr :: (sizeT ~ SizeT) => Value sizeT -> Value sizeT -> Value sizeT
elemOfArr (FinV i) (ArrV xs) = xs !! i
elemOfArr i arr = error $ printf "invalid inputs: elemOfArr[ix: %s, arr: %s]" (show i) (show arr)

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
evalBasicExpr VarE{var} = view $ _state . Ctx.at var . non' (error $ "cannot find variable " <> var)
evalBasicExpr DefaultE{ty} = return $ defaultV ty
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
evalBasicExpr ProjectE{tup_expr, tup_ix_val} = do
  arr <- evalBasicExpr tup_expr
  return $ arr ^?! _TupV . ix tup_ix_val

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
  deriving (Generic, HasDefault)

type instance SizeType (EvaluationEnv primsT sizeT) = sizeT
type instance PrimitiveType (EvaluationEnv primsT sizeT) = primsT

class HasEvaluationEnv p where
  _evaluationEnv ::
    (primsT ~ PrimitiveType p, sizeT ~ SizeType p) =>
    Lens' p (EvaluationEnv primsT sizeT)

instance HasEvaluationEnv (EvaluationEnv primsT sizeT) where _evaluationEnv = id

instance HasFunCtx (EvaluationEnv primsT sizeT) where
  _funCtx focus (EvaluationEnv f fi) = focus f <&> \f' -> EvaluationEnv f' fi

instance HasFunInterpCtx (EvaluationEnv primsT sizeT) where
  _funInterpCtx focus (EvaluationEnv f fi) = focus fi <&> EvaluationEnv f

-- ================================================================================
-- Program Execution
-- ================================================================================

type ExecutionState sizeT = ProgramState sizeT

-- | Base probability monad to evaluate the program.
type EvaluationMonad precT = Prob.ExpMonad precT

-- | Non-deterministic Execution Monad (i.e. no state)
type Evaluator primsT sizeT precT = ReaderT (EvaluationEnv primsT sizeT) (EvaluationMonad precT)

-- | Non-deterministic Execution Monad
type Executor primsT sizeT precT = StateT (ExecutionState sizeT) (Evaluator primsT sizeT precT)

-- --------------------------------------------------------------------------------
-- Primitives (with generics)
-- --------------------------------------------------------------------------------

{- | Primitives that support evaluation:
 Can evaluate @primT@ under a context of @primsT@.
-}
class (Fractional precT) => EvaluatablePrimitive primT precT where
  evalPrimitive ::
    ( sizeT ~ SizeT
    , m ~ Evaluator primsT sizeT precT
    , EvaluatablePrimitive primsT precT
    ) =>
    primT ->
    ProgramState sizeT ->
    m [Value sizeT]
  default evalPrimitive ::
    ( Generic primT
    , GEvaluatablePrimitive (Rep primT) precT
    , sizeT ~ SizeT
    , m ~ Evaluator primsT sizeT precT
    , EvaluatablePrimitive primsT precT
    ) =>
    primT ->
    ProgramState sizeT ->
    m [Value sizeT]
  evalPrimitive x = gevalPrimitive (from x)

instance (Fractional precT) => EvaluatablePrimitive Void precT where
  evalPrimitive = absurd

class GEvaluatablePrimitive f precT where
  gevalPrimitive ::
    ( sizeT ~ SizeT
    , m ~ Evaluator primsT sizeT precT
    , EvaluatablePrimitive primsT precT
    ) =>
    f primT ->
    ProgramState sizeT ->
    m [Value sizeT]

instance
  (GEvaluatablePrimitive f1 precT, GEvaluatablePrimitive f2 precT) =>
  GEvaluatablePrimitive (f1 :+: f2) precT
  where
  gevalPrimitive (L1 p) = gevalPrimitive p
  gevalPrimitive (R1 p) = gevalPrimitive p

instance
  (GEvaluatablePrimitive f precT) =>
  GEvaluatablePrimitive (M1 i c f) precT
  where
  gevalPrimitive (M1 x) = gevalPrimitive x

instance
  (EvaluatablePrimitive f precT) =>
  GEvaluatablePrimitive (K1 i f) precT
  where
  gevalPrimitive (K1 x) = evalPrimitive x

-- --------------------------------------------------------------------------------
-- Core Language
-- --------------------------------------------------------------------------------

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

evalRandomSampleExpr ::
  ( MonadReader env m
  , HasProgramState env
  , sizeT ~ SizeType env
  , sizeT ~ SizeT
  , Prob.MonadProb precT m
  , Fractional precT
  ) =>
  DistrExpr sizeT ->
  m (Value sizeT)
evalRandomSampleExpr UniformE{sample_ty} = Prob.uniform (domain sample_ty)
evalRandomSampleExpr BernoulliE{prob_one} = toValue <$> Prob.bernoulli (realToFrac prob_one)

evalExpr ::
  forall primsT precT m.
  ( EvaluatablePrimitive primsT precT
  , m ~ Evaluator primsT SizeT precT
  , Prob.ProbType precT
  ) =>
  Expr primsT SizeT ->
  ProgramState SizeT ->
  m [Value SizeT]
evalExpr BasicExprE{basic_expr} sigma = do
  val <- runReaderT ?? sigma $ evalBasicExpr basic_expr
  return [val]

-- probabilistic instructions
evalExpr RandomSampleE{distr_expr} sigma = do
  val <- runReaderT ?? sigma $ evalRandomSampleExpr distr_expr
  return [val]

-- function calls
evalExpr FunCallE{fname, args} sigma = do
  arg_vals <- evalStateT ?? sigma $ mapM lookupS args
  fun_def <- view $ _funCtx . Ctx.at fname . singular _Just
  evalFun arg_vals (NamedFunDef fname fun_def)

-- subroutines
evalExpr PrimCallE{prim} sigma = do
  evalPrimitive prim sigma

-- loop
evalExpr LoopE{initial_args, loop_body_fun} sigma = do
  fun_def <- view $ _funCtx . Ctx.at loop_body_fun . singular _Just
  init_vals <- concat <$> mapM (\e -> evalExpr (BasicExprE e) sigma) initial_args
  foldM (\args i -> evalFun (args ++ [i]) (NamedFunDef loop_body_fun fun_def)) init_vals (domain (last (param_types fun_def)))

execStmt ::
  forall primsT precT m.
  ( EvaluatablePrimitive primsT precT
  , m ~ Executor primsT SizeT precT
  , Prob.ProbType precT
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
  forall primsT precT m.
  ( EvaluatablePrimitive primsT precT
  , m ~ Evaluator primsT SizeT precT
  , Prob.ProbType precT
  ) =>
  -- | arguments
  [Value SizeT] ->
  -- | function
  NamedFunDef primsT SizeT ->
  m [Value SizeT]
evalFun vals_in NamedFunDef{fun_def = FunDef{mbody = Just FunBody{param_names, ret_names, body_stmt}}, fun_name} = do
  when (length param_names /= length vals_in) $ error (printf "incorrect number of fun `%s` args: expected %s, got %s" fun_name (show param_names) (show vals_in))
  let params = Ctx.fromList $ zip param_names vals_in
  (evalStateT ?? params) $ do
    execStmt body_stmt
    mapM lookupS ret_names
evalFun vals_in NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} = do
  fn_interp <- view $ _funInterpCtx . Ctx.at fun_name . singular _Just
  return $ fn_interp vals_in

runProgram ::
  forall primsT precT.
  ( EvaluatablePrimitive primsT precT
  , Prob.ProbType precT
  ) =>
  Program primsT SizeT ->
  FunInterpCtx SizeT ->
  [Value SizeT] ->
  EvaluationMonad precT [Value SizeT]
runProgram (Program fs) funInterpCtx inp =
  evalFun inp (last fs) & (runReaderT ?? env)
 where
  env =
    default_
      & (_funCtx .~ namedFunsToFunCtx fs)
      & (_funInterpCtx .~ funInterpCtx)

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.ProtoLang.Eval (
  -- * Evaluating Basic Expressions
  ProgramState,
  HasProgramState (..),
  evalBasicExpr,

  -- * Evaluations
  runProgram,

  -- * Values
  defaultV,
  validateValueType,
  CoerceValue (..),
  toValue,
  fromValue,
  valueToBool,
  domainSize,
  bitsize,
  domain,

  -- * Types and Monad
  EvaluationMonad,
  Evaluatable (..),
  EvalReqs,
  EvalArgs,
  eval1,

  -- ** Evaluation
  FunInterp,
  FunInterpCtx,
  HasFunInterpCtx (..),
  EvaluationEnv,
  HasEvaluationEnv (..),
  Evaluator,
) where

import Control.Monad (foldM, replicateM, when)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, evalStateT)
import Data.Bits (Bits (xor))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
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
defaultV (Bitvec _) = FinV 0
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

instance CoerceValue Int where
  safeToValue n = pure $ FinV n

  safeFromValue (FinV n) = pure n
  safeFromValue _ = fail "cannot convert non-Fin to Int"

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
domainSize :: (Integral sizeT) => VarType sizeT -> sizeT
domainSize (Fin _N) = _N
domainSize (Bitvec n) = 2 ^ n
domainSize (Arr n t) = domainSize t ^ n
domainSize (Tup ts) = product $ map domainSize ts

-- | Set of all values of a given type
domain :: (Integral sizeT) => VarType sizeT -> [Value sizeT]
domain (Fin _N) = FinV <$> [0 .. _N - 1]
domain (Bitvec n) = FinV <$> [0 .. 2 ^ n - 1]
domain (Arr n t) = ArrV <$> replicateM (fromIntegral n) (domain t)
domain (Tup ts) = TupV <$> traverse domain ts

-- | Bitsize
bitsize :: (Num size) => VarType size -> Maybe size
bitsize (Fin _) = Nothing
bitsize (Bitvec n) = Just n
bitsize (Arr n t) = (n *) <$> bitsize t
bitsize (Tup ts) = sum <$> mapM bitsize ts

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
evalBinOp EqOp (FinV x) (FinV y) = toValue $ x == y
evalBinOp AndOp v1 v2 = toValue $ fromValue v1 && fromValue v2
evalBinOp op lhs rhs = error $ printf "evalBinOp failed: %s (%s) (%s)" (show op) (show lhs) (show rhs)

evalOp :: (sizeT ~ SizeT) => NAryOp -> [Value sizeT] -> Value sizeT
evalOp MultiOrOp = toValue . any valueToBool

-- | @elemOfArr i a@ returns a[i]
elemOfArr :: (sizeT ~ SizeT) => Value sizeT -> Value sizeT -> Value sizeT
elemOfArr (FinV i) (ArrV xs) = xs !! i
elemOfArr (FinV i) (TupV xs) = xs !! i
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

-- | A mapping of data intepretations
type FunInterpCtx sizeT = Map.Map Ident (FunInterp sizeT)

class HasFunInterpCtx p where
  _funInterpCtx :: (sizeT ~ SizeType p) => Lens' p (FunInterpCtx sizeT)

instance HasFunInterpCtx (FunInterpCtx sizeT) where _funInterpCtx = id

-- | Environment for evaluation
data EvaluationEnv ext = EvaluationEnv (FunCtx ext) (FunInterpCtx (SizeType ext))
  deriving (Generic, HasDefault)

type instance SizeType (EvaluationEnv ext) = SizeType ext
type instance PrecType (EvaluationEnv ext) = PrecType ext

class HasEvaluationEnv p ext | p -> ext where
  _evaluationEnv :: Lens' p (EvaluationEnv ext)

instance HasEvaluationEnv (EvaluationEnv ext) ext where _evaluationEnv = id

instance HasFunCtx (EvaluationEnv ext) ext where
  _funCtx focus (EvaluationEnv f fi) = focus f <&> \f' -> EvaluationEnv f' fi

instance HasFunInterpCtx (EvaluationEnv ext) where
  _funInterpCtx focus (EvaluationEnv f fi) = focus fi <&> EvaluationEnv f

-- ================================================================================
-- Program Evaluation
-- ================================================================================

-- | Base probability monad to evaluate the program.
type EvaluationMonad precT = Prob.ExpMonad precT

-- | Non-deterministic evaluation monad (i.e. no state)
type Evaluator ext = ReaderT (EvaluationEnv ext) (EvaluationMonad (PrecType ext))

-- --------------------------------------------------------------------------------
-- Primitives (with generics)
-- --------------------------------------------------------------------------------

-- | Constraints to be satisfied to support evaluation.
type EvalReqs sizeT precT =
  ( sizeT ~ SizeT
  , Prob.ProbType precT
  , Prob.RVType precT precT
  , Fractional precT
  )

-- | Primitives that support evaluation.
class
  (EvalReqs sizeT precT, sizeT ~ SizeType ext, precT ~ PrecType ext) =>
  Evaluatable ext sizeT precT
    | ext -> sizeT precT
  where
  eval ::
    forall ext' m.
    ( Evaluatable ext' sizeT precT
    , m ~ Evaluator ext'
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    ext ->
    ProgramState sizeT ->
    m [Value sizeT]
  default eval ::
    forall ext' m.
    ( Generic ext
    , GEvaluatable (Rep ext) sizeT precT
    , Evaluatable ext' sizeT precT
    , m ~ Evaluator ext'
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    ext ->
    ProgramState sizeT ->
    m [Value sizeT]
  eval x = geval (from x)

instance (EvalReqs SizeT precT) => Evaluatable (Core SizeT precT) SizeT precT where
  eval p = case p of {}

class GEvaluatable f sizeT precT | f -> sizeT precT where
  geval ::
    forall ext' m ext.
    ( m ~ Evaluator ext'
    , Evaluatable ext' sizeT precT
    ) =>
    f ext ->
    ProgramState sizeT ->
    m [Value sizeT]

instance (GEvaluatable f1 sizeT precT, GEvaluatable f2 sizeT precT) => GEvaluatable (f1 :+: f2) sizeT precT where
  geval (L1 p) = geval p
  geval (R1 p) = geval p

instance (GEvaluatable f sizeT precT) => GEvaluatable (M1 i c f) sizeT precT where
  geval (M1 x) = geval x

instance (Evaluatable f sizeT precT) => GEvaluatable (K1 i f) sizeT precT where
  geval (K1 x) = eval x

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
lookupS x = use $ _state . Ctx.at x . non' (error $ "cannot find variable " ++ x)

evalRandomSampleExpr ::
  ( MonadReader env m
  , HasProgramState env
  , sizeT ~ SizeType env
  , Prob.MonadProb precT m
  , EvalReqs sizeT precT
  ) =>
  DistrExpr sizeT ->
  m (Value sizeT)
evalRandomSampleExpr UniformE{sample_ty} = Prob.uniform (domain sample_ty)
evalRandomSampleExpr BernoulliE{prob_one} = toValue <$> Prob.bernoulli (realToFrac prob_one)

class Eval1 f where
  type EvalArgs f ext
  type EvalRets f ext

  eval1 ::
    forall ext size prec m.
    ( Evaluatable ext size prec
    , m ~ Evaluator ext
    ) =>
    f ext ->
    EvalArgs f ext ->
    m (EvalRets f ext)

instance Eval1 Expr where
  type EvalArgs Expr ext = ProgramState (SizeType ext)
  type EvalRets Expr ext = [Value (SizeType ext)]

  -- deterministic expressions
  eval1 BasicExprE{basic_expr} sigma = do
    val <- runReaderT ?? sigma $ evalBasicExpr basic_expr
    return [val]

  -- probabilistic expressions
  eval1 RandomSampleE{distr_expr} sigma = do
    val <- runReaderT ?? sigma $ evalRandomSampleExpr distr_expr
    return [val]

  -- function calls
  eval1 FunCallE{fname, args} sigma = do
    arg_vals <- evalStateT ?? sigma $ mapM lookupS args
    fun_def <- view $ _funCtx . Ctx.at fname . singular _Just
    eval1 (NamedFunDef fname fun_def) arg_vals

  -- subroutines
  eval1 PrimCallE{prim} sigma = do eval prim sigma

  -- loop
  eval1 LoopE{initial_args, loop_body_fun} sigma = do
    fun_def <- view $ _funCtx . Ctx.at loop_body_fun . singular _Just
    init_vals <- evalStateT ?? sigma $ mapM lookupS initial_args
    foldM (\args i -> eval1 (NamedFunDef loop_body_fun fun_def) (args ++ [i])) init_vals (domain (last (param_types fun_def)))

instance Eval1 Stmt where
  type EvalArgs Stmt ext = ProgramState (SizeType ext)
  type EvalRets Stmt ext = ProgramState (SizeType ext)

  eval1 ExprS{rets, expr} sigma = do
    vals <- eval1 expr sigma
    let sigma' = foldr (\(x, v) -> Ctx.ins x .~ v) sigma (zip rets vals)
    return sigma'
  eval1 IfThenElseS{cond, s_true, s_false} sigma = do
    let cond_val = sigma ^. Ctx.at cond . non' (error $ "cannot find " <> cond)
    let s = if fromValue cond_val then s_true else s_false
    eval1 s sigma
  eval1 (SeqS ss) sigma = foldM (flip eval1) sigma ss

instance Eval1 FunBody where
  type EvalArgs FunBody ext = [Value (SizeType ext)]
  type EvalRets FunBody ext = [Value (SizeType ext)]

  eval1 FunBody{param_names, ret_names, body_stmt} vals_in = do
    when (length param_names /= length vals_in) $ error (printf "incorrect number of args: expected %s, got %s" (show param_names) (show vals_in))
    let params = Ctx.fromList $ zip param_names vals_in
    sigma' <- eval1 body_stmt params
    (evalStateT ?? sigma') $ mapM lookupS ret_names

instance Eval1 NamedFunDef where
  type EvalArgs NamedFunDef ext = [Value (SizeType ext)]
  type EvalRets NamedFunDef ext = [Value (SizeType ext)]

  -- defined function: run the body
  eval1 NamedFunDef{fun_def = FunDef{mbody = Just body}} vals_in = do
    eval1 body vals_in

  -- external function: lookup and run the provided interpretation
  eval1 NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} vals_in = do
    fn_interp <- view $ _funInterpCtx . at fun_name . non' (error $ "could not find fun interp for " ++ fun_name)
    return $ fn_interp vals_in

instance Eval1 Program where
  type EvalArgs Program ext = [Value (SizeType ext)]
  type EvalRets Program ext = [Value (SizeType ext)]

  eval1 (Program fs) = eval1 (last fs)

-- ================================================================================
-- Entry Points
-- ================================================================================

-- | Entry-point: run the program (i.e. last function)
runProgram ::
  forall ext precT m.
  ( Evaluatable ext SizeT precT
  , EvalReqs SizeT precT
  , m ~ EvaluationMonad precT
  ) =>
  Program ext ->
  FunInterpCtx SizeT ->
  [Value SizeT] ->
  m [Value SizeT]
runProgram p funInterpCtx inp = eval1 p inp & (runReaderT ?? env)
 where
  env =
    default_
      & (_funCtx .~ programToFunCtx p)
      & (_funInterpCtx .~ funInterpCtx)

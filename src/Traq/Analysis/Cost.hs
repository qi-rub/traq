{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Traq.Analysis.Cost (
  module Traq.Analysis.Cost.Prelude,
  module Traq.Analysis.Cost.Unitary,
  module Traq.Analysis.Cost.Quantum,

  -- * Old cost functions

  -- * Unitary Cost
  unitaryQueryCost,
  unitaryQueryCostF,
  UnitaryCost (..),

  -- * Quantum Cost

  -- ** Quantum Worst case Cost
  quantumMaxQueryCost,
  quantumMaxQueryCostF,
  QuantumHavocCost (..),

  -- * Types and Monad
  CostEnv,
  _costEnv,
  CostCalculator,

  -- * Precision Splitting Strategies
  PrecisionSplittingStrategy (..),
  HasPrecisionSplittingStrategy (..),
  HasNeedsEps,
  splitEps,
) where

import Control.Monad (forM, zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import GHC.Generics hiding (to)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Cost.Prelude
import Traq.Analysis.Cost.Quantum
import Traq.Analysis.Cost.Unitary
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Analysis.Error
import Traq.Analysis.Prelude
import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck

{-# DEPRECATED
  unitaryQueryCost
  , unitaryQueryCostE
  , unitaryQueryCostS
  , unitaryQueryCostF
  , quantumMaxQueryCost
  , quantumMaxQueryCostE
  , quantumMaxQueryCostS
  , quantumMaxQueryCostF
  , UnitaryCost
  , QuantumHavocCost
  "Use the newer versions on annotated programs"
  #-}

-- ================================================================================
-- Strategy for splitting the precison (eps/delta)
-- ================================================================================

-- | Predicate for checking if an expr/stmt/prog can fail
class HasNeedsEps p where
  type ExtensionType p

  needsEps ::
    ( sizeT ~ SizeType p
    , ext ~ ExtensionType p
    , MonadReader env m
    , HasFunCtx env ext
    ) =>
    p ->
    m Bool

instance HasNeedsEps (Expr ext) where
  type ExtensionType (Expr ext) = ext

  needsEps FunCallE{fname} =
    view (_funCtx . Ctx.at fname . singular _Just) >>= needsEps
  needsEps PrimCallE{} = return True
  needsEps _ = return False

instance HasNeedsEps (Stmt ext) where
  type ExtensionType (Stmt ext) = ext

  needsEps ExprS{expr} = needsEps expr
  needsEps (SeqS ss) = or <$> mapM needsEps ss
  needsEps IfThenElseS{s_true, s_false} = (||) <$> needsEps s_true <*> needsEps s_false

instance HasNeedsEps (FunDef ext) where
  type ExtensionType (FunDef ext) = ext

  needsEps FunDef{mbody} = case mbody of
    Nothing -> return False
    Just FunBody{body_stmt} -> needsEps body_stmt

splitEps ::
  forall primT sizeT precT errT env m.
  ( Floating precT
  , DivideError errT precT
  , MonadReader env m
  , HasFunCtx env primT
  , HasPrecisionSplittingStrategy env
  , HasNeedsEps (Stmt primT)
  , sizeT ~ SizeType env
  ) =>
  errT ->
  [Stmt primT] ->
  m [errT]
splitEps _ [] = return []
splitEps eps [_] = return [eps]
splitEps eps ss = do
  view _precSplitStrat >>= \case
    SplitSimple -> split_simple
    SplitUsingNeedsEps -> split_using_need_eps
 where
  split_simple = do
    let epss = eps & iterate (`divideError` 2) & tail & take (length ss - 1)
    return $ epss ++ [last epss]

  split_using_need_eps = do
    flags <- forM ss $ \s -> needsEps s
    let n_fail = length $ filter id flags

    let eps_each = if n_fail == 0 then 0 else eps `divideError` fromIntegral n_fail
    return $ map (\flag -> if flag then eps_each else 0) flags

-- ================================================================================
-- Env & Monad for Cost Analysis
-- ================================================================================

-- | Environment for cost analysis
data CostEnv ext = CostEnv (EvaluationEnv ext) PrecisionSplittingStrategy
  deriving (Generic, HasDefault)

-- Types and instances
type instance SizeType (CostEnv ext) = SizeType ext
type instance PrecType (CostEnv ext) = PrecType ext

instance HasEvaluationEnv (CostEnv ext) ext where
  _evaluationEnv focus (CostEnv e strat) = focus e <&> \e' -> CostEnv e' strat

instance HasFunCtx (CostEnv ext) ext where _funCtx = _evaluationEnv . _funCtx
instance HasFunInterpCtx (CostEnv ext) where _funInterpCtx = _evaluationEnv . _funInterpCtx

instance HasPrecisionSplittingStrategy (CostEnv ext) where
  _precSplitStrat focus (CostEnv f s) = focus s <&> \s' -> CostEnv f s'

-- Subtyping Lens
class HasCostEnv p ext | p -> ext where
  _costEnv :: Lens' p (CostEnv ext)

instance HasCostEnv (CostEnv ext) ext where _costEnv = id

-- | Monad for cost analysis
type CostCalculator ext = ReaderT (CostEnv ext) Maybe

-- ================================================================================
-- Unitary Cost
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Unitary Cost: Primitives (with generics)
-- --------------------------------------------------------------------------------

-- | Primitives that have a unitary cost
class
  (TypeInferrable primT sizeT, sizeT ~ SizeType primT, precT ~ PrecType primT) =>
  UnitaryCost primT sizeT precT
    | primT -> sizeT precT
  where
  unitaryCost ::
    forall primsT costT m.
    ( m ~ CostCalculator primsT
    , UnitaryCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    ) =>
    L2NormError precT ->
    primT ->
    m costT
  default unitaryCost ::
    forall primsT costT m.
    ( m ~ CostCalculator primsT
    , UnitaryCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Generic primT
    , GUnitaryCost (Rep primT) sizeT precT
    ) =>
    L2NormError precT ->
    primT ->
    m costT
  unitaryCost delta p = gunitaryCost delta (from p)

instance (TypingReqs sizeT) => UnitaryCost (Core sizeT precT) sizeT precT where
  unitaryCost _ = \case {}

class GUnitaryCost f sizeT precT where
  gunitaryCost ::
    forall primsT primT costT m.
    ( m ~ CostCalculator primsT
    , UnitaryCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    ) =>
    L2NormError precT ->
    f primT ->
    m costT

instance
  (GUnitaryCost f1 sizeT precT, GUnitaryCost f2 sizeT precT) =>
  GUnitaryCost (f1 :+: f2) sizeT precT
  where
  gunitaryCost delta (L1 p) = gunitaryCost delta p
  gunitaryCost delta (R1 p) = gunitaryCost delta p

instance (GUnitaryCost f sizeT precT) => GUnitaryCost (M1 i c f) sizeT precT where
  gunitaryCost delta (M1 x) = gunitaryCost delta x

instance
  (UnitaryCost f sizeT precT) =>
  GUnitaryCost (K1 i f) sizeT precT
  where
  gunitaryCost delta (K1 x) = unitaryCost delta x

-- --------------------------------------------------------------------------------
-- Unitary Cost: Core Language
-- --------------------------------------------------------------------------------

-- | Evaluate the query cost of an expression
unitaryQueryCostE ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision
  L2NormError precT ->
  -- | expression @E@
  Expr primsT ->
  m costT
unitaryQueryCostE delta FunCallE{fname} = ((2.0 :: precT) Alg..*) <$> unitaryQueryCostF (delta `divideError` 2) fname
unitaryQueryCostE delta PrimCallE{prim} = unitaryCost delta prim
-- basic expressions
unitaryQueryCostE _ BasicExprE{basic_expr} = return $ C.callExpr C.Unitary basic_expr
unitaryQueryCostE _ RandomSampleE{distr_expr} = return $ C.callDistrExpr C.Unitary distr_expr
unitaryQueryCostE _ _ = error "TODO"

-- Evaluate the query cost of a statement
unitaryQueryCostS ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision (l2-norm)
  L2NormError precT ->
  -- | statement @S@
  Stmt primsT ->
  m costT
unitaryQueryCostS delta ExprS{expr} = unitaryQueryCostE delta expr
unitaryQueryCostS delta IfThenElseS{s_true, s_false} = do
  cost_true <- unitaryQueryCostS delta s_true
  cost_false <- unitaryQueryCostS delta s_false
  return $ cost_true Alg.+ cost_false
unitaryQueryCostS delta (SeqS ss) = do
  delta_each <- splitEps delta ss
  cs <- zipWithM unitaryQueryCostS delta_each ss
  return $ Alg.sum cs

-- Evaluate the query cost of a function
unitaryQueryCostF ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision (l2-norm)
  L2NormError precT ->
  -- | function name
  Ident ->
  m costT
-- declaration: use tick value (or 0 if not specified)
unitaryQueryCostF delta fname = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . non' (error $ "invalid function: " ++ fname)
  case mbody of
    Nothing -> return $ C.query C.Unitary fname -- query an external function
    Just FunBody{body_stmt} -> unitaryQueryCostS delta body_stmt -- def: compute using body

unitaryQueryCost ::
  forall primsT sizeT precT costT.
  ( Num sizeT
  , Floating precT
  , UnitaryCost primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  PrecisionSplittingStrategy ->
  -- | precision (l2-norm)
  L2NormError precT ->
  -- | program @P@
  Program primsT ->
  costT
unitaryQueryCost strat delta (Program fs) =
  let env =
        default_
          & (_funCtx .~ namedFunsToFunCtx fs)
          & (_precSplitStrat .~ strat)
   in unitaryQueryCostF delta (fun_name $ last fs)
        & (runReaderT ?? env)
        & (^?! _Just)

-- ================================================================================
-- Quantum Max Cost
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Quantum Max Cost: Primitives (with generics)
-- --------------------------------------------------------------------------------

-- | Primitives that have a quantum max cost
class
  (UnitaryCost primT sizeT precT) =>
  QuantumHavocCost primT sizeT precT
  where
  quantumHavocCost ::
    forall primsT costT m.
    ( m ~ CostCalculator primsT
    , QuantumHavocCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    ) =>
    FailProb precT ->
    primT ->
    m costT
  default quantumHavocCost ::
    forall primsT costT m.
    ( m ~ CostCalculator primsT
    , QuantumHavocCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    , Generic primT
    , GQuantumHavocCost (Rep primT) sizeT precT
    ) =>
    FailProb precT ->
    primT ->
    m costT
  quantumHavocCost eps p = gquantumHavocCost eps (from p)

instance (TypingReqs sizeT) => QuantumHavocCost (Core sizeT precT) sizeT precT where
  quantumHavocCost _ = \case {}

class GQuantumHavocCost f sizeT precT where
  gquantumHavocCost ::
    forall primT primsT costT m.
    ( m ~ CostCalculator primsT
    , QuantumHavocCost primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    ) =>
    FailProb precT ->
    f primT ->
    m costT

instance
  (GQuantumHavocCost f1 sizeT precT, GQuantumHavocCost f2 sizeT precT) =>
  GQuantumHavocCost (f1 :+: f2) sizeT precT
  where
  gquantumHavocCost eps (L1 p) = gquantumHavocCost eps p
  gquantumHavocCost eps (R1 p) = gquantumHavocCost eps p

instance (GQuantumHavocCost f sizeT precT) => GQuantumHavocCost (M1 i c f) sizeT precT where
  gquantumHavocCost eps (M1 x) = gquantumHavocCost eps x

instance
  (QuantumHavocCost f sizeT precT) =>
  GQuantumHavocCost (K1 i f) sizeT precT
  where
  gquantumHavocCost eps (K1 x) = quantumHavocCost eps x

-- --------------------------------------------------------------------------------
-- Quantum Max Cost: Core Language
-- --------------------------------------------------------------------------------

quantumMaxQueryCostE ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumHavocCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  FailProb precT ->
  -- | statement @S@
  Expr primsT ->
  m costT
quantumMaxQueryCostE eps FunCallE{fname} = quantumMaxQueryCostF eps fname
-- known cost formulas
quantumMaxQueryCostE eps PrimCallE{prim} =
  quantumHavocCost eps prim
-- basic expressions
quantumMaxQueryCostE _ BasicExprE{basic_expr} = return $ C.callExpr C.Classical basic_expr
quantumMaxQueryCostE _ RandomSampleE{distr_expr} = return $ C.callDistrExpr C.Classical distr_expr
quantumMaxQueryCostE _ _ = error "TODO"

quantumMaxQueryCostS ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumHavocCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  FailProb precT ->
  -- | statement @S@
  Stmt primsT ->
  m costT
quantumMaxQueryCostS eps ExprS{expr} = quantumMaxQueryCostE eps expr
quantumMaxQueryCostS eps IfThenElseS{s_true, s_false} =
  max <$> quantumMaxQueryCostS eps s_true <*> quantumMaxQueryCostS eps s_false
quantumMaxQueryCostS eps (SeqS ss) = do
  eps_each <- splitEps eps ss
  cs <- zipWithM quantumMaxQueryCostS eps_each ss
  return $ Alg.sum cs

quantumMaxQueryCostF ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumHavocCost primsT sizeT precT
  , m ~ CostCalculator primsT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  FailProb precT ->
  -- | function name
  Ident ->
  m costT
quantumMaxQueryCostF eps fname = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . singular _Just
  case mbody of
    Nothing -> return $ C.query C.Classical fname -- query an external function
    Just FunBody{body_stmt} -> quantumMaxQueryCostS eps body_stmt -- def: compute using body

quantumMaxQueryCost ::
  forall primsT sizeT precT costT.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumHavocCost primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | failure probability `eps`
  FailProb precT ->
  -- | program `P`
  Program primsT ->
  costT
quantumMaxQueryCost strat a_eps (Program fs) =
  let env =
        default_
          & (_funCtx .~ namedFunsToFunCtx fs)
          & (_precSplitStrat .~ strat)
   in quantumMaxQueryCostF a_eps (fun_name $ last fs)
        & (runReaderT ?? env)
        & (^?! _Just)

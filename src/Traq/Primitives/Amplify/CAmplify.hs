{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.CAmplify (
  CAmplify (..),

  -- * Symbolic formulas
  _QMax,
  _EQ,
)
where

import GHC.Generics (Generic)

import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- | Classical (probabilistic) bounded repetition.
newtype CAmplify size prec = CAmplify (Amplify size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (CAmplify size prec) = size
type instance PrecType (CAmplify size prec) = prec

type instance PrimFnShape (CAmplify size prec) = SamplerFn

instance Amplify size prec :<: CAmplify size prec

instance P.MapSize (CAmplify size prec) where
  type MappedSize (CAmplify size prec) size' = CAmplify size' prec
  mapSize f (CAmplify p) = CAmplify (P.mapSize f p)

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (CAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = CAmplify <$> parsePrimParams tp name
  printPrimParams (CAmplify prim) = printPrimParams prim

instance (P.TypingReqs size) => TypeCheckPrim (CAmplify size prec) size where
  inferRetTypesPrim (CAmplify p) = inferRetTypesPrim p

instance (P.EvalReqs size prec, Ord prec) => EvalPrim (CAmplify size prec) size prec where
  evalPrim (CAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Maximum queries
_QMax :: forall prec. (Floating prec) => A.FailProb prec -> prec -> prec
_QMax eps p_min = logBase (1 / (1 - p_min)) (1 / A.getFailProb eps)

-- | Expected queries
_EQ :: forall prec. (Floating prec, Ord prec) => A.FailProb prec -> prec -> prec -> prec
_EQ eps p_min p_good
  | p_good >= p_min = 1 / p_good
  | p_good == 0 = _QMax eps p_min
  | otherwise = error "invalid case: 0 < p_good < p_min"

instance (P.TypingReqs size, Floating prec) => UnitaryCostPrim (CAmplify size prec) size prec where
  unitaryQueryCosts (CAmplify Amplify{p_min}) eps = SamplerFn $ weakQueries $ _QMax eps p_min
  unitaryExprCosts = Alg.zero

instance (P.TypingReqs size, A.SizeToPrec size prec, Floating prec) => QuantumHavocCostPrim (CAmplify size prec) size prec where
  quantumQueryCostsQuantum (CAmplify Amplify{p_min}) eps = SamplerFn $ _QMax eps p_min

  -- no unitary cost for classical algo
  quantumQueryCostsUnitary _ _ = SamplerFn zeroQ

  quantumExprCosts = Alg.zero

instance (P.EvalReqs size prec, Floating prec, Ord prec) => QuantumExpCostPrim (CAmplify size prec) size prec where
  quantumExpQueryCostsQuantum (CAmplify Amplify{p_min}) eps (SamplerFn eval_sample) = SamplerFn [([], _EQ eps p_min p_succ)]
   where
    mu = eval_sample []
    p_succ = Prob.probabilityOf success mu

    -- check if sampling produced a good sample.
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

  -- no unitary cost for classical algo
  quantumExpQueryCostsUnitary _ _ _ = SamplerFn zeroQ

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

instance UnitaryCompilePrim (CAmplify size prec) size prec where
  compileUPrim (CAmplify Amplify{}) eps = do
    error "TODO: CompileU CAmplify"

instance QuantumCompilePrim (CAmplify size prec) size prec where
  compileQPrim (CAmplify Amplify{}) eps = do
    error "TODO: CompileQ CAmplify"

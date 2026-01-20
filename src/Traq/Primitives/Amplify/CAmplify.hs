{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
newtype CAmplify sizeT precT = CAmplify (Amplify sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (CAmplify sizeT precT) = sizeT
type instance PrecType (CAmplify sizeT precT) = precT

type instance PrimFnShape (CAmplify size prec) = SamplerFn

instance Amplify sizeT precT :<: CAmplify sizeT precT

instance P.MapSize (CAmplify size prec) where
  type MappedSize (CAmplify size prec) size' = CAmplify size' prec
  mapSize f (CAmplify p) = CAmplify (P.mapSize f p)

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (CAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = CAmplify <$> parsePrimParams tp name
  printPrimParams (CAmplify prim) = printPrimParams prim

instance (P.TypingReqs sizeT) => TypeCheckPrim (CAmplify sizeT precT) sizeT where
  inferRetTypesPrim (CAmplify p) = inferRetTypesPrim p

instance (P.EvalReqs sizeT precT, Ord precT) => EvalPrim (CAmplify sizeT precT) sizeT precT where
  evalPrim (CAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Maximum queries
_QMax :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_QMax eps p_min = logBase (1 / (1 - p_min)) (1 / A.getFailProb eps)

-- | Expected queries
_EQ :: forall precT. (Floating precT, Ord precT) => A.FailProb precT -> precT -> precT -> precT
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

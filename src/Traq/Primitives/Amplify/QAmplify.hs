{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.QAmplify (
  QAmplify (..),

  -- * Query Formulas
  _FPAA_L,
  _WQSearch,
  _EQSearch,
) where

import GHC.Generics (Generic)

import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- | Quantum Amplitude Amplification.
newtype QAmplify sizeT precT = QAmplify (Amplify sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (QAmplify sizeT precT) = sizeT
type instance PrecType (QAmplify sizeT precT) = precT

type instance PrimFnShape (QAmplify size prec) = SamplerFn

instance Amplify sizeT precT :<: QAmplify sizeT precT

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (QAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = QAmplify <$> parsePrimParams tp name
  printPrimParams (QAmplify prim) = printPrimParams prim

instance (P.TypingReqs sizeT) => TypeCheckPrim (QAmplify sizeT precT) sizeT where
  inferRetTypesPrim (QAmplify p) = inferRetTypesPrim p

instance (P.EvalReqs sizeT precT, Ord precT) => EvalPrim (QAmplify sizeT precT) sizeT precT where
  evalPrim (QAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Fixed-Point Amplitude Amplification.
_FPAA_L :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_FPAA_L eps p_min = acosh (1 / sqrt (A.getFailProb eps)) / acosh (1 / sqrt (1 - p_min))

instance (P.TypingReqs size, Floating prec) => UnitaryCostPrim (QAmplify size prec) size prec where
  unitaryQueryCosts (QAmplify Amplify{p_min}) eps = SamplerFn $ weakQueries $ _FPAA_L eps p_min
  unitaryExprCosts _ _ = Alg.zero

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 4 of https://arxiv.org/abs/2203.04975
-}
_WQSearch :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_WQSearch eps p_min = alpha * log (1 / A.getFailProb eps) / sqrt p_min
 where
  alpha = 9.2

-- | Eq. 3 of https://arxiv.org/abs/2203.04975
_F :: forall precT. (Floating precT, Ord precT) => precT -> precT
_F p_good
  | p_good >= 0.25 = 2.0344
  | otherwise = alpha / (3 * sqrt p_good)
 where
  alpha = 9.2

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 2 of https://arxiv.org/abs/2203.04975
-}
_EQSearch :: forall precT. (Floating precT, Ord precT) => A.FailProb precT -> precT -> precT -> precT
_EQSearch eps p_min p_good
  | p_good == 0 = _WQSearch eps p_min
  | otherwise = _F p_good * (1 + 1 / (1 - term))
 where
  term = _F p_good * sqrt p_min / alpha
  alpha = 9.2

instance (P.TypingReqs size, A.SizeToPrec size prec, Floating prec) => QuantumHavocCostPrim (QAmplify size prec) size prec where
  quantumQueryCostsUnitary (QAmplify Amplify{p_min}) eps = SamplerFn $ strongQueries $ _WQSearch eps p_min
  quantumQueryCostsQuantum _ _ = SamplerFn 0

instance (P.EvalReqs size prec, Floating prec, Ord prec) => QuantumExpCostPrim (QAmplify size prec) size prec where
  quantumExpQueryCostsUnitary (QAmplify Amplify{p_min}) eps (SamplerFn eval_sample) = SamplerFn $ strongQueries $ _EQSearch eps p_min p_good
   where
    mu = eval_sample []
    p_good = Prob.probabilityOf success mu

    -- check if sampling produced a good sample.
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

  quantumExpQueryCostsQuantum _ _ _ = SamplerFn []

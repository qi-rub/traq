{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Simons.Quantum (
  -- * Primitive
  SimonsFindXorPeriod (..),

  -- * Query Formulas
  _SimonsQueries,
) where

import GHC.Generics (Generic)

import Traq.Data.Subtyping

import qualified Traq.Analysis as P
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Prelude
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Primitive and Query Cost Formulas
-- ================================================================================

{- |
Simon's Quantum Algorithm with an approximate promise ([1], Theorem 1)

References:

 1. [Breaking Symmetric Cryptosystems using Quantum Period Finding](https://arxiv.org/pdf/1602.05973)
-}
newtype SimonsFindXorPeriod sizeT precT = SimonsFindXorPeriod (FindXorPeriod sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (SimonsFindXorPeriod sizeT precT) = sizeT
type instance PrecType (SimonsFindXorPeriod sizeT precT) = precT

type instance PrimFnShape (SimonsFindXorPeriod sizeT precT) = FindXorPeriodArg

instance P.MapSize (SimonsFindXorPeriod size prec) where
  type MappedSize (SimonsFindXorPeriod size prec) size' = (SimonsFindXorPeriod size' prec)
  mapSize f (SimonsFindXorPeriod p) = SimonsFindXorPeriod (P.mapSize f p)

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance FindXorPeriod sizeT precT :<: SimonsFindXorPeriod sizeT precT

instance IsA (FindXorPeriod sizeT precT) (SimonsFindXorPeriod sizeT precT)

instance (Show sizeT) => SerializePrim (SimonsFindXorPeriod sizeT Double) where
  primNames = ["findXorPeriod"]
  parsePrimParams tp s = SimonsFindXorPeriod <$> parsePrimParams tp s
  printPrimParams (SimonsFindXorPeriod prim) = printPrimParams prim

instance
  (P.TypingReqs size, Num prec, Ord prec, Show prec) =>
  TypeCheckPrim (SimonsFindXorPeriod size prec) size
  where
  inferRetTypesPrim (SimonsFindXorPeriod p) = inferRetTypesPrim p

-- ================================================================================
-- Cost Instances
-- ================================================================================

-- | Number of queries as described in Theorem 1.
_SimonsQueries ::
  forall sizeT precT.
  (Floating precT, P.SizeToPrec sizeT precT) =>
  -- | bitsize
  sizeT ->
  -- | p_0: maximum probability of spurious collisions for non-period values.
  precT ->
  -- | maximum allowed failure probability.
  P.FailProb precT ->
  precT
_SimonsQueries n p0 eps = q
 where
  {- Sketch:
    We need to pick @c@ such that @(2 * ((1 + p0)/2)^c)^n <= eps@ (see Theorem 1)
    ==> 2^n / eps <= (2 / (1+p0))^(cn)
    define q := cn (i.e. number of queries)
    ==> n + log_2 (1/eps) <= q * log_2(2 / (1+p0))
  -}

  q_num = P.sizeToPrec n + logBase 2 (1 / P.getFailProb eps)
  q_den = logBase 2 (2 / (1 + p0))
  q = q_num / q_den

instance
  (P.TypingReqs size, Floating prec, Ord prec, Show prec, P.SizeToPrec size prec) =>
  UnitaryCostPrim (SimonsFindXorPeriod size prec) size prec
  where
  unitaryQueryCosts prim eps =
    let FindXorPeriod{n, p_0} = extract prim :: FindXorPeriod size prec
     in FindXorPeriodArg{fun = strongQueries $ _SimonsQueries n p_0 eps}

-- | Same as unitary compilation.
instance
  (P.TypingReqs size, Floating prec, Ord prec, Show prec, P.SizeToPrec size prec) =>
  QuantumHavocCostPrim (SimonsFindXorPeriod size prec) size prec
  where
  quantumQueryCostsQuantum _ _ = FindXorPeriodArg{fun = 0}

  quantumQueryCostsUnitary prim eps =
    let FindXorPeriod{n, p_0} = extract prim :: FindXorPeriod size prec
     in FindXorPeriodArg{fun = strongQueries $ _SimonsQueries n p_0 eps}

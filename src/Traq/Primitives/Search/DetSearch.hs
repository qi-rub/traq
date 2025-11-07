{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.DetSearch (
  DetSearch (..),
) where

import GHC.Generics (Generic)

import Lens.Micro.GHC
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

-- | Primitive implementing brute-force classical search.
newtype DetSearch sizeT precT = DetSearch (PrimSearch sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (DetSearch sizeT precT) = sizeT
type instance PrecType (DetSearch sizeT precT) = precT

type instance PrimFnShape (DetSearch size prec) = BooleanPredicate

instance P.MapSize (DetSearch size prec) where
  type MappedSize (DetSearch size prec) size' = DetSearch size' prec
  mapSize f (DetSearch p) = DetSearch (P.mapSize f p)

instance PrimSearch sizeT precT :<: DetSearch sizeT precT

instance (Show size) => SerializePrim (DetSearch size prec) where
  primNames = ["any_det"]
  parsePrimParams tp s = DetSearch <$> parsePrimParams tp s
  printPrimParams (DetSearch prim) = printPrimParams prim

instance (P.TypingReqs size) => TypeCheckPrim (DetSearch size prec) size where
  inferRetTypesPrim (DetSearch prim) = inferRetTypesPrim prim

instance EvalPrim (DetSearch size prec) size prec where
  evalPrim (DetSearch prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance (P.TypingReqs size, Integral size, Num prec) => UnitaryCostPrim (DetSearch size prec) size prec where
  unitaryQueryCosts (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate{predicate = fromIntegral _N}
   where
    _N = P.domainSize search_ty

instance (P.TypingReqs size, Integral size, Num prec, P.SizeToPrec size prec) => QuantumHavocCostPrim (DetSearch size prec) size prec where
  -- only classical queries
  quantumQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate{predicate = fromIntegral _N}
   where
    _N = P.domainSize search_ty

  -- no unitary
  quantumQueryCostsUnitary _ _ = BooleanPredicate{predicate = 0}

instance
  (size ~ SizeT, Floating prec, Alg.Monoidal prec, Alg.Semiring prec) =>
  QuantumExpCostPrim (DetSearch size prec) size prec
  where
  quantumExpQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ BooleanPredicate{predicate = eval_pred} =
    BooleanPredicate{predicate = [([v], 1) | v <- queried_vals]}
   where
    results =
      P.domain search_ty <&> \v ->
        let Just [b] = Prob.toDeterministicValue $ eval_pred [v]
         in (P.valueToBool b, v)

    -- query all values till the first solution.
    (non_sols, sol_and_rest) = break fst results & (each %~ map snd)
    queried_vals = non_sols ++ take 1 sol_and_rest

  quantumExpQueryCostsUnitary _ _ _ = BooleanPredicate{predicate = 0}

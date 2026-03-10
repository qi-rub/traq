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

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

-- | Primitive implementing brute-force classical search.
newtype DetSearch size prec = DetSearch (PrimSearch size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (DetSearch size prec) = size
type instance PrecType (DetSearch size prec) = prec

type instance PrimFnShape (DetSearch size prec) = BooleanPredicate

instance CPL.MapSize (DetSearch size prec) where
  type MappedSize (DetSearch size prec) size' = DetSearch size' prec
  mapSize f (DetSearch p) = DetSearch (CPL.mapSize f p)

instance PrimSearch size prec :<: DetSearch size prec

instance (Show size) => SerializePrim (DetSearch size prec) where
  primNames = ["any"]
  parsePrimParams tp s = DetSearch <$> parsePrimParams tp s
  printPrimParams (DetSearch prim) = printPrimParams prim

instance (CPL.TypingReqs size) => TypeCheckPrim (DetSearch size prec) size where
  inferRetTypesPrim (DetSearch prim) = inferRetTypesPrim prim

instance EvalPrim (DetSearch size prec) size prec where
  evalPrim (DetSearch prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance (CPL.TypingReqs size, Integral size, Num prec) => UnitaryCostPrim (DetSearch size prec) size prec where
  unitaryQueryCosts (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate (weakQueries $ fromIntegral _N)
   where
    _N = CPL.domainSize search_ty

  unitaryExprCosts _ _ = Alg.zero

instance (CPL.TypingReqs size, Integral size, Num prec, A.SizeToPrec size prec) => QuantumHavocCostPrim (DetSearch size prec) size prec where
  -- only classical queries
  quantumQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate (fromIntegral _N)
   where
    _N = CPL.domainSize search_ty

  -- no unitary
  quantumQueryCostsUnitary _ _ = BooleanPredicate $ weakQueries 0

  quantumExprCosts = Alg.zero

instance
  (size ~ SizeT, Floating prec, Alg.Monoidal prec, Alg.Semiring prec) =>
  QuantumExpCostPrim (DetSearch size prec) size prec
  where
  quantumExpQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ (BooleanPredicate eval_pred) =
    BooleanPredicate [([v], 1) | v <- queried_vals]
   where
    results =
      CPL.domain search_ty <&> \v ->
        case Prob.toDeterministicValue $ eval_pred [v] of
          Just [b] -> (CPL.valueToBool b, v)
          _ -> error "predicate is not determinisic"

    -- query all values till the first solution.
    (non_sols, sol_and_rest) = break fst results & (each %~ map snd)
    queried_vals = non_sols ++ take 1 sol_and_rest

  quantumExpQueryCostsUnitary _ _ _ = BooleanPredicate $ weakQueries 0

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

instance UnitaryCompilePrim (DetSearch size prec) size prec where
  compileUPrim (DetSearch PrimSearch{search_kind, search_ty}) eps = do
    error "TODO: CompileU DetSearch"

instance QuantumCompilePrim (DetSearch size prec) size prec where
  compileQPrim (DetSearch PrimSearch{search_kind, search_ty}) eps = do
    error "TODO: CompileQ DetSearch"

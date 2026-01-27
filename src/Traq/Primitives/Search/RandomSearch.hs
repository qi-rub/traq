{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import GHC.Generics (Generic)

import Lens.Micro.GHC
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob

import qualified Traq.Analysis as P
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- | Number of predicate queries to unitarily implement random search.
_URandomSearch :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> precT
_URandomSearch = fromIntegral

-- | Worst case number of predicate queries to implement random search.
_ERandomSearchWorst :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> P.FailProb precT -> precT
_ERandomSearchWorst n eps = fromIntegral n * log (1 / P.getFailProb eps)

-- | Expected number of predicate queries to implement random search.
_ERandomSearch :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> sizeT -> P.FailProb precT -> precT
_ERandomSearch n 0 eps = _ERandomSearchWorst n eps
_ERandomSearch n k _ = fromIntegral n / fromIntegral k

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

{- | Primitive implementing search using classical random sampling.
 The unitary mode does a brute-force loop.
-}
newtype RandomSearch sizeT precT = RandomSearch (PrimSearch sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (RandomSearch sizeT precT) = sizeT
type instance PrecType (RandomSearch sizeT precT) = precT

type instance PrimFnShape (RandomSearch size prec) = BooleanPredicate

instance P.MapSize (RandomSearch size prec) where
  type MappedSize (RandomSearch size prec) size' = RandomSearch size' prec
  mapSize f (RandomSearch p) = RandomSearch (P.mapSize f p)

instance (Show size) => SerializePrim (RandomSearch size prec) where
  primNames = ["any"]
  parsePrimParams tp s = RandomSearch <$> parsePrimParams tp s
  printPrimParams (RandomSearch prim) = printPrimParams prim

instance (P.TypingReqs size) => TypeCheckPrim (RandomSearch size prec) size where
  inferRetTypesPrim (RandomSearch prim) = inferRetTypesPrim prim

instance EvalPrim (RandomSearch size prec) size prec where
  evalPrim (RandomSearch prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance (P.TypingReqs size, Integral size, Floating prec) => UnitaryCostPrim (RandomSearch size prec) size prec where
  unitaryQueryCosts (RandomSearch PrimSearch{search_ty}) _ = BooleanPredicate $ weakQueries $ _URandomSearch _N
   where
    _N = P.domainSize search_ty

  unitaryExprCosts _ _ = Alg.zero

instance (P.TypingReqs size, Integral size, Floating prec, P.SizeToPrec size prec) => QuantumHavocCostPrim (RandomSearch size prec) size prec where
  -- only classical queries
  quantumQueryCostsQuantum (RandomSearch PrimSearch{search_ty}) eps =
    BooleanPredicate $ _ERandomSearchWorst _N eps
   where
    _N = P.domainSize search_ty

  -- no unitary
  quantumQueryCostsUnitary _ _ = BooleanPredicate zeroQ

  quantumExprCosts = Alg.zero

instance
  (size ~ SizeT, Floating prec, Alg.Monoidal prec, Alg.Semiring prec) =>
  QuantumExpCostPrim (RandomSearch size prec) size prec
  where
  quantumExpQueryCostsQuantum (RandomSearch PrimSearch{search_ty}) eps (BooleanPredicate eval_pred) =
    BooleanPredicate [([v], if b then qry_wt_per_sol else qry_wt_per_non_sol) | (b, v) <- results]
   where
    _N = P.domainSize search_ty

    results =
      P.domain search_ty <&> \v ->
        case Prob.toDeterministicValue $ eval_pred [v] of
          Just [b] -> (P.valueToBool b, v)
          _ -> error "predicate is not determinisic"

    _K = length $ filter fst results
    qry = _ERandomSearch _N _K eps

    qry_wt_per_sol = 1.0 / fromIntegral _K
    qry_wt_per_non_sol = qry / fromIntegral (_N - _K)

  quantumExpQueryCostsUnitary _ _ _ = BooleanPredicate $ weakQueries 0

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

instance UnitaryCompilePrim (RandomSearch size prec) size prec where
  compileUPrim (RandomSearch PrimSearch{search_kind, search_ty}) eps = do
    error "TODO: CompileU RandomSearch"

instance QuantumCompilePrim (RandomSearch size prec) size prec where
  compileQPrim (RandomSearch PrimSearch{search_kind, search_ty}) eps = do
    error "TODO: CompileQ RandomSearch"

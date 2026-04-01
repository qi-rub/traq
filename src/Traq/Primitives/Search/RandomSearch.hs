{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import Control.Monad.Except (throwError)
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import qualified Traq.QPL as QPL

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- | Number of predicate queries to unitarily implement random search.
_URandomSearch :: forall size prec. (Integral size, Floating prec) => size -> prec
_URandomSearch = fromIntegral

-- | Worst case number of predicate queries to implement random search.
_ERandomSearchWorst :: forall size prec. (Integral size, Floating prec) => size -> A.FailProb prec -> prec
_ERandomSearchWorst n eps = fromIntegral n * log (1 / A.getFailProb eps)

-- | Expected number of predicate queries to implement random search.
_ERandomSearch :: forall size prec. (Integral size, Floating prec) => size -> size -> A.FailProb prec -> prec
_ERandomSearch n 0 eps = _ERandomSearchWorst n eps
_ERandomSearch n k _ = fromIntegral n / fromIntegral k

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

{- | Primitive implementing search using classical random sampling.
 The unitary mode does a brute-force loop.
-}
newtype RandomSearch size prec = RandomSearch (PrimSearch size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (RandomSearch size prec) = size
type instance PrecType (RandomSearch size prec) = prec

type instance PrimFnShape (RandomSearch size prec) = BooleanPredicate

instance CPL.MapSize (RandomSearch size prec) where
  type MappedSize (RandomSearch size prec) size' = RandomSearch size' prec
  mapSize f (RandomSearch p) = RandomSearch (CPL.mapSize f p)

instance CPL.MapPrec (RandomSearch size prec) where
  type MappedPrec (RandomSearch size prec) prec' = RandomSearch size prec'
  mapPrec f (RandomSearch p) = RandomSearch (CPL.mapPrec f p)

instance (Show size) => SerializePrim (RandomSearch size prec) where
  primNames = ["any"]
  parsePrimParams tp s = RandomSearch <$> parsePrimParams tp s
  printPrimParams (RandomSearch prim) = printPrimParams prim

instance (CPL.TypingReqs size) => TypeCheckPrim (RandomSearch size prec) size where
  inferRetTypesPrim (RandomSearch prim) = inferRetTypesPrim prim

instance EvalPrim (RandomSearch size prec) size prec where
  evalPrim (RandomSearch prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance (CPL.TypingReqs size, Integral size, Floating prec) => UnitaryCostPrim (RandomSearch size prec) size prec where
  unitaryQueryCosts (RandomSearch PrimSearch{search_ty}) _ = BooleanPredicate $ weakQueries $ _URandomSearch _N
   where
    _N = CPL.domainSize search_ty

  unitaryExprCosts _ _ = Alg.zero

instance (CPL.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec) => QuantumHavocCostPrim (RandomSearch size prec) size prec where
  -- only classical queries
  quantumQueryCostsQuantum (RandomSearch PrimSearch{search_ty}) eps =
    BooleanPredicate $ _ERandomSearchWorst _N eps
   where
    _N = CPL.domainSize search_ty

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
    _N = CPL.domainSize search_ty

    results =
      CPL.domain search_ty <&> \v ->
        case Prob.toDeterministicValue $ eval_pred [v] of
          Just [b] -> (CPL.valueToBool b, v)
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

instance (Integral size) => UnitaryCompilePrim (RandomSearch size prec) size prec where
  compileUPrim (RandomSearch p) = compileUPrim (DetSearch p)

instance (Integral size, RealFrac prec, Floating prec) => QuantumCompilePrim (RandomSearch size prec) size prec where
  compileQPrim (RandomSearch PrimSearch{search_ty, search_kind}) eps = do
    ret_tys <- view $ to prim_ret_types
    (flag_ty, sample_ty) <- case ret_tys of
      [b, t] -> return (b, t)
      [b] -> return (b, search_ty)
      _ -> throwError "typecheck failed"

    flag <- Compiler.newIdent "ok"
    x_out <- Compiler.newIdent "x"

    (BooleanPredicate call_pred) <- view $ to mk_call

    let _N = CPL.domainSize search_ty
    let qmax = _ERandomSearchWorst _N eps

    let ret_params = case search_kind of
          SearchK -> [(flag, flag_ty), (x_out, sample_ty)]
          _ -> [(flag, flag_ty)]

    Compiler.buildProc "RandSearch" [] ret_params $ do
      sample <- case search_kind of
        SearchK -> return x_out
        _ -> Compiler.allocLocalWithPrefix "x" search_ty
      Compiler.withStmt (QPL.RepeatS (QPL.MetaSize (ceiling qmax))) $ do
        Compiler.addStmt $
          QPL.IfThenElseS
            { cond = flag
            , s_true = QPL.SkipS
            , s_false =
                QPL.SeqS
                  [ QPL.RandomS [sample] (CPL.UniformE sample_ty)
                  , call_pred [QPL.Arg flag, QPL.Arg sample]
                  ]
            }

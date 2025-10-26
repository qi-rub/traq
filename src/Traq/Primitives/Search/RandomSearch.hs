{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Traq.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import Control.Monad (forM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import Traq.Prelude
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- | Number of predicate queries to unitarily implement random search.
_URandomSearch :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> precT
{-# HLINT ignore "Eta reduce" #-}
_URandomSearch n = fromIntegral n

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
newtype RandomSearch sizeT precT = RandomSearch (PrimAny sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (RandomSearch sizeT precT) = sizeT
type instance PrecType (RandomSearch sizeT precT) = precT

instance P.MapSize (RandomSearch size prec) where
  type MappedSize (RandomSearch size prec) size' = RandomSearch size' prec
  mapSize f (RandomSearch p) = RandomSearch (P.mapSize f p)

instance PrimAny sizeT precT :<: RandomSearch sizeT precT
instance IsA SearchLikePrim (RandomSearch sizeT precT)

instance PP.ToCodeString (RandomSearch sizeT precT) where
  build (RandomSearch p) = printSearchLikePrim "any" p

instance P.Parseable (RandomSearch sizeT precT) where
  parseE = fmap RandomSearch . parsePrimAnyWithName "any"

instance P.HasFreeVars (RandomSearch sizeT precT)
instance (P.TypeCheckable sizeT) => P.TypeCheckablePrimitive (RandomSearch sizeT precT) sizeT

instance (P.EvalReqs sizeT precT) => P.Evaluatable (RandomSearch sizeT precT) sizeT precT

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance
  (P.TypeCheckable sizeT, Integral sizeT, Floating precT) =>
  P.UnitaryCostablePrimitive (RandomSearch sizeT precT) sizeT precT
  where
  unitaryQueryCostPrimitive delta prim = do
    let SearchLikePrim{predicate} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- number of predicate queries
    let qry = _URandomSearch n

    -- precision per predicate call
    let delta_per_pred_call = delta `P.divideError` qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry Alg..* cost_pred

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , P.TypeCheckable sizeT
  ) =>
  P.QuantumMaxCostablePrimitive (RandomSearch sizeT precT) sizeT precT
  where
  quantumMaxQueryCostPrimitive eps prim = do
    let SearchLikePrim{predicate} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps `P.divideError` 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let max_qry = _ERandomSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred `P.divideError` max_qry

    -- cost of each predicate call
    cost_pred_call <-
      P.quantumMaxQueryCostE eps_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ max_qry Alg..* cost_pred_call

average :: forall precT a. (Alg.Monoidal a, Alg.Module precT a, Floating precT) => [a] -> a
average [] = Alg.zero
average xs =
  let n = length xs
   in ((1.0 :: precT) / fromIntegral n) Alg..* Alg.sum xs

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , P.EvalReqs sizeT precT
  ) =>
  P.QuantumCostablePrimitive (RandomSearch sizeT precT) sizeT precT
  where
  quantumQueryCostPrimitive eps prim sigma = do
    let SearchLikePrim{predicate, pred_args = args} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    ty@(P.Fin n) <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps `P.divideError` 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let max_qry = _ERandomSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred `P.divideError` max_qry

    arg_vals <- runReaderT ?? sigma $ forM args $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")
    let sigma_pred = Ctx.fromList $ zip ["in" ++ show i | i <- [1 .. length args]] arg_vals

    costs <- forM (P.domain ty) $ \v -> do
      let sigma_pred' = sigma_pred & Ctx.ins "x_s" .~ v
      let pred_call_expr =
            P.FunCallE
              { P.fname = predicate
              , P.args = Ctx.keys sigma_pred'
              }

      -- cost of predicate on input `v`
      cost_v <- P.quantumQueryCostE eps_per_pred_call sigma_pred' pred_call_expr

      -- evaluate predicate on `v` to check if it is a solution
      eval_env <- view P._evaluationEnv
      [is_sol_v] <-
        lift $
          P.eval pred_call_expr sigma_pred'
            & (runReaderT ?? eval_env)
            & Prob.toDeterministicValue
      return (P.valueToBool is_sol_v, cost_v)

    -- number of solutions
    let k = length $ filter fst costs
    let qry = _ERandomSearch n k eps_search

    -- average costs of a solution and a non-solution respectively
    let avg_sol_cost = average @precT . map snd . filter fst $ costs
    let avg_non_sol_cost = average @precT . map snd . filter (not . fst) $ costs

    return $ qry Alg..* avg_non_sol_cost Alg.+ avg_sol_cost

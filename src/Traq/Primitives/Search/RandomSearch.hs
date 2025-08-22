{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Traq.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import Control.Monad (forM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- | Number of predicate queries to unitarily implement random search.
_URandomSearch :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT
{-# HLINT ignore "Eta reduce" #-}
_URandomSearch n = fromIntegral n

-- | Worst case number of predicate queries to implement random search.
_ERandomSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_ERandomSearchWorst n eps = fromIntegral n * log (1 / eps)

-- | Expected number of predicate queries to implement random search.
_ERandomSearch :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT -> costT
_ERandomSearch n 0 eps = _ERandomSearchWorst n eps
_ERandomSearch n k _ = fromIntegral n / fromIntegral k

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

{- | Primitive implementing search using classical random sampling.
 The unitary mode does a brute-force loop.
-}
data RandomSearch = RandomSearch {predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

instance HasPrimAny RandomSearch where
  _PrimAny focus (RandomSearch p args) = uncurry RandomSearch <$> focus (p, args)
  mkPrimAny = RandomSearch

instance P.HasFreeVars RandomSearch where
  freeVarsList RandomSearch{args} = args

instance PP.ToCodeString RandomSearch where
  build RandomSearch{predicate, args} = PP.putWord $ printf "@any[%s](%s)" predicate (PP.commaList args)

instance P.CanParsePrimitive RandomSearch where
  primitiveParser = parsePrimAny "any"

instance P.TypeCheckablePrimitive RandomSearch sizeT where
  typeCheckPrimitive = typeCheckPrimAny

instance
  ( Fractional costT
  , P.EvaluatablePrimitive primsT primsT costT
  ) =>
  P.EvaluatablePrimitive primsT RandomSearch costT
  where
  evalPrimitive = evaluatePrimAny

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT RandomSearch sizeT costT
  where
  unitaryQueryCostPrimitive delta RandomSearch{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- number of predicate queries
    let qry = _URandomSearch n

    -- precision per predicate call
    let delta_per_pred_call = delta / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry * cost_pred

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT RandomSearch sizeT costT
  where
  quantumMaxQueryCostPrimitive eps RandomSearch{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let max_qry = _ERandomSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / max_qry

    -- cost of each predicate call
    cost_pred_call <-
      P.quantumMaxQueryCostE eps_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ max_qry * cost_pred_call

average :: (Floating a) => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (length xs)

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT RandomSearch sizeT costT
  where
  quantumQueryCostPrimitive eps RandomSearch{predicate, args} sigma = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    ty@(P.Fin n) <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let max_qry = _ERandomSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / max_qry

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
          P.evalExpr @primsT @costT pred_call_expr sigma_pred'
            & (runReaderT ?? eval_env)
            & Prob.toDeterministicValue
      return (P.valueToBool is_sol_v, cost_v)

    -- number of solutions
    let k = length $ filter fst costs
    let qry = _ERandomSearch n k eps_search

    -- average costs of a solution and a non-solution respectively
    let avg_sol_cost = average . map snd . filter fst $ costs
    let avg_non_sol_cost = average . map snd . filter (not . fst) $ costs

    return $ qry * avg_non_sol_cost + avg_sol_cost

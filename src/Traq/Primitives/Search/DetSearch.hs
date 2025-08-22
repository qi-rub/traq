{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Search.DetSearch (
  DetSearch (..),
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
-- Primitive Implementation
-- ================================================================================

-- | Primitive implementing brute-force classical search.
data DetSearch = DetSearch {predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

instance HasPrimAny DetSearch where
  mkPrimAny = DetSearch
  _PrimAny focus (DetSearch p args) = uncurry DetSearch <$> focus (p, args)

instance P.HasFreeVars DetSearch where
  freeVarsList DetSearch{args} = args

instance PP.ToCodeString DetSearch where
  build DetSearch{predicate} = PP.putWord $ printf "@any[%s]" predicate

instance P.CanParsePrimitive DetSearch where
  primitiveParser = parsePrimAny "any"

instance P.TypeCheckablePrimitive DetSearch where
  typeCheckPrimitive = typeCheckPrimAny

instance
  ( P.EvaluatablePrimitive primsT primsT costT
  , Fractional costT
  ) =>
  P.EvaluatablePrimitive primsT DetSearch costT
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
  P.UnitaryCostablePrimitive primsT DetSearch sizeT costT
  where
  unitaryQueryCostPrimitive delta DetSearch{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- precision per predicate call
    let delta_per_pred_call = delta / fromIntegral n

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ fromIntegral n * cost_pred

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT DetSearch sizeT costT
  where
  quantumMaxQueryCostPrimitive eps DetSearch{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- fail prob per predicate call
    let eps_per_pred_call = eps / fromIntegral n

    -- cost of each predicate call
    cost_pred_call <-
      P.quantumMaxQueryCostE eps_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ fromIntegral n * cost_pred_call

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT DetSearch sizeT costT
  where
  quantumQueryCostPrimitive eps DetSearch{predicate, args} sigma = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    ty@(P.Fin n) <- pure $ last param_types

    -- fail prob per predicate call
    let eps_per_pred_call = eps / fromIntegral n

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
        P.evalExpr @primsT @costT pred_call_expr sigma_pred'
          & (runReaderT ?? eval_env)
          & Prob.toDeterministicValue
          & lift

      return (P.valueToBool is_sol_v, cost_v)

    -- average costs of a solution and a non-solution respectively
    let (non_sols, sol_and_rest) = break fst costs & (each %~ map snd)
    let sol_cost = case sol_and_rest of [] -> 0; (c : _) -> c

    return $ sum non_sols + sol_cost

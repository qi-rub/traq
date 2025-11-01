{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.DetSearch (
  DetSearch (..),
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
-- Primitive Implementation
-- ================================================================================

-- | Primitive implementing brute-force classical search.
newtype DetSearch sizeT precT = DetSearch (PrimAny sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (DetSearch sizeT precT) = sizeT
type instance PrecType (DetSearch sizeT precT) = precT

instance P.MapSize (DetSearch size prec) where
  type MappedSize (DetSearch size prec) size' = DetSearch size' prec
  mapSize f (DetSearch p) = DetSearch (P.mapSize f p)

instance PrimAny sizeT precT :<: DetSearch sizeT precT
instance IsA SearchLikePrim (DetSearch sizeT precT)

instance PP.ToCodeString (DetSearch sizeT precT) where
  build (DetSearch p) = printSearchLikePrim "any" p

instance P.Parseable (DetSearch sizeT precT) where
  parseE = fmap inject . parsePrimAnyWithName @sizeT @precT "any"

instance P.HasFreeVars (DetSearch sizeT precT)
instance (P.TypingReqs sizeT) => P.TypeInferrable (DetSearch sizeT precT) sizeT

instance (P.EvalReqs sizeT precT) => P.Evaluatable (DetSearch sizeT precT) sizeT precT

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance
  ( Integral sizeT
  , Floating precT
  , P.TypingReqs sizeT
  ) =>
  P.UnitaryCost (DetSearch sizeT precT) sizeT precT
  where
  unitaryCost delta prim = do
    let SearchLikePrim{predicate} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- precision per predicate call
    let delta_per_pred_call = delta `P.divideError` fromIntegral n

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ (P.sizeToPrec n :: precT) Alg..* cost_pred

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , P.TypingReqs sizeT
  ) =>
  P.QuantumHavocCost (DetSearch sizeT precT) sizeT precT
  where
  quantumHavocCost eps prim = do
    let SearchLikePrim{predicate} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- fail prob per predicate call
    let eps_per_pred_call = eps `P.divideError` fromIntegral n

    -- cost of each predicate call
    cost_pred_call <-
      P.quantumMaxQueryCostE eps_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ (P.sizeToPrec n :: precT) Alg..* cost_pred_call

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , Prob.ProbType precT
  , sizeT ~ SizeT
  , P.EvalReqs sizeT precT
  ) =>
  P.QuantumExpCost (DetSearch sizeT precT) sizeT precT
  where
  quantumExpCost eps prim sigma = do
    let SearchLikePrim{predicate, pred_args = args} = extract prim

    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    ty@(P.Fin n) <- pure $ last param_types

    -- fail prob per predicate call
    let eps_per_pred_call = eps `P.divideError` fromIntegral n

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
        P.eval pred_call_expr sigma_pred'
          & (runReaderT ?? eval_env)
          & Prob.toDeterministicValue
          & lift

      return (P.valueToBool is_sol_v, cost_v)

    -- average costs of a solution and a non-solution respectively
    let (non_sols, sol_and_rest) = break fst costs & (each %~ map snd)
    let sol_cost = case sol_and_rest of [] -> Alg.zero; (c : _) -> c

    return $ Alg.sum non_sols Alg.+ sol_cost

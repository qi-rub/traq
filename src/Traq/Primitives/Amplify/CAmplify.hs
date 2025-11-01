{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.CAmplify (
  CAmplify (..),

  -- * Symbolic formulas
  _QMax,
  _EQ,
)
where

import Control.Monad (forM)
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- | Classical (probabilistic) bounded repetition.
newtype CAmplify sizeT precT = CAmplify (Amplify sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (CAmplify sizeT precT) = sizeT
type instance PrecType (CAmplify sizeT precT) = precT

instance Amplify sizeT precT :<: CAmplify sizeT precT

-- Inherited instances
instance PP.ToCodeString (CAmplify sizeT Double) where
  build (CAmplify p) = PP.build p

instance P.Parseable (CAmplify sizeT Double) where
  parseE tp = CAmplify <$> P.parseE tp

instance P.HasFreeVars (CAmplify sizeT precT)
instance (P.TypingReqs sizeT, Num precT, Ord precT, Show precT) => P.TypeInferrable (CAmplify sizeT precT) sizeT

instance (Ord precT, P.EvalReqs sizeT precT) => P.Evaluatable (CAmplify sizeT precT) sizeT precT

_QMax :: forall precT. (Floating precT) => P.FailProb precT -> precT -> precT
_QMax eps p_min = logBase (1 / (1 - p_min)) (1 / P.getFailProb eps)

_EQ :: forall precT. (Floating precT, Ord precT) => P.FailProb precT -> precT -> precT -> precT
_EQ eps p_min p_good
  | p_good >= p_min = 1 / p_good
  | p_good == 0 = _QMax eps p_min
  | otherwise = error "invalid case: 0 < p_good < p_min"

instance
  ( Integral sizeT
  , Floating precT
  , Eq precT
  , Ord precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  P.UnitaryCost (CAmplify sizeT precT) sizeT precT
  where
  unitaryCost delta (CAmplify (Amplify{sampler, p_min})) = do
    let delta_a = delta `P.divideError` 2
    let qry = _QMax (P.requiredNormErrorToFailProb delta_a) p_min
    let delta_f = (delta - delta_a) `P.divideError` qry

    cost_sampler <-
      P.unitaryQueryCostE delta_f $
        P.FunCallE{P.fname = sampler, P.args = undefined}

    return $ qry Alg..* cost_sampler

instance
  ( Integral sizeT
  , Floating precT
  , Eq precT
  , Ord precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  P.QuantumHavocCost (CAmplify sizeT precT) sizeT precT
  where
  quantumHavocCost eps (CAmplify (Amplify{sampler, p_min})) = do
    let eps_a = eps `P.divideError` 2
    let num_repetitions = _QMax eps_a p_min

    let eps_f = (eps - eps_a) `P.divideError` num_repetitions

    cost_sampler <-
      P.quantumMaxQueryCostE eps_f $
        P.FunCallE{P.fname = sampler, P.args = undefined}

    return $ num_repetitions Alg..* cost_sampler

instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , Eq precT
  , Ord precT
  , Show precT
  , P.EvalReqs sizeT precT
  ) =>
  P.QuantumExpCost (CAmplify sizeT precT) sizeT precT
  where
  quantumExpCost eps (CAmplify (Amplify{sampler, p_min, sampler_args})) sigma = do
    -- get the function identifier
    sampler_fundef <-
      view $
        P._funCtx
          . Ctx.at sampler
          . non' (error "unable to find sampler, please typecheck first!")

    -- get current evaluation environment
    eval_env <- view P._evaluationEnv

    -- get argument values
    arg_vals <- runReaderT ?? sigma $ forM sampler_args $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")

    -- calculate result distribution μ
    let mu =
          P.evalFun arg_vals (P.NamedFunDef sampler sampler_fundef)
            & (runReaderT ?? eval_env)

    let p_succ = Prob.probabilityOf success mu
    let expected = _EQ eps p_min p_succ -- Expected Cost
    let eps_a = eps `P.divideError` 2
    let num_repetitions = _QMax eps_a p_min
    let eps_f = (eps - eps_a) `P.divideError` num_repetitions

    cost <- P.quantumQueryCostE eps_f sigma P.FunCallE{P.fname = sampler, P.args = sampler_args}

    return $ expected Alg..* cost
   where
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

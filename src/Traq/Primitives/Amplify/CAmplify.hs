{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify.CAmplify (
  CAmplify (..),
  -- Symbolic formulas
  _QryClassicalU,
  _QryClassicalMax,
)
where

import Control.Monad (forM)
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude (SizeT)
import Traq.Primitives.Amplify.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- | Classical (probabilistic) bounded repetition.
newtype CAmplify = CAmplify Amplify
  deriving (Eq, Show, Read, Generic)

instance Amplify :<: CAmplify

-- Inherited instances
instance PP.ToCodeString CAmplify where
  build (CAmplify p) = PP.build p

instance P.CanParsePrimitive CAmplify where
  primitiveParser tp = CAmplify <$> P.primitiveParser tp

instance P.HasFreeVars CAmplify
instance P.TypeCheckablePrimitive CAmplify

instance (Ord precT) => P.Evaluatable CAmplify precT

_QryClassicalU :: forall precT. (Show precT) => Sym.Sym precT -> Double -> Sym.Sym precT
_QryClassicalU eps p_min = Sym.var $ printf "QryU_Amplify(%s, %s)" (show eps) (show p_min)

_QryClassicalMax :: forall precT. (Show precT) => Sym.Sym precT -> Double -> Sym.Sym precT
_QryClassicalMax eps p_min = Sym.var $ printf "QMAX_Amplify(%s, %s)" (show eps) (show p_min)

_EQ :: forall precT. (Fractional precT, Show precT, Ord precT) => Sym.Sym precT -> Double -> Sym.Sym precT -> Sym.Sym precT
_EQ eps p_min p_good
  | p_good >= realToFrac p_min = 1 / p_good
  | p_good == 0 = _QryClassicalMax eps p_min
  | otherwise = error "invalid case: 0 < p_good < p_min"

instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , Eq precT
  , Ord precT
  , precT' ~ Sym.Sym precT
  ) =>
  P.UnitaryCostablePrimitive CAmplify sizeT precT'
  where
  unitaryQueryCostPrimitive delta (CAmplify (Amplify{sampler, p_min})) = do
    let delta_a = delta / 2
    let qry = _QryClassicalU delta_a p_min
    let delta_f = (delta - delta_a) / qry

    cost_sampler <-
      P.unitaryQueryCostE delta_f $
        P.FunCallE{P.fname = sampler, P.args = undefined}

    return $ qry Alg..* cost_sampler

instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , Eq precT
  , Ord precT
  , precT' ~ Sym.Sym precT
  ) =>
  P.QuantumMaxCostablePrimitive CAmplify sizeT precT'
  where
  quantumMaxQueryCostPrimitive eps (CAmplify (Amplify{sampler, p_min})) = do
    let eps_a = eps / 2
    let num_repetitions = _QryClassicalMax eps_a p_min

    let eps_f = (eps - eps_a) / num_repetitions

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
  , precT' ~ Sym.Sym precT
  , P.Evaluatable CAmplify precT'
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive CAmplify sizeT precT'
  where
  quantumQueryCostPrimitive eps (CAmplify (Amplify{sampler, p_min, sampler_args})) sigma = do
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
          P.evalFun @_ @precT' arg_vals (P.NamedFunDef sampler sampler_fundef)
            & (runReaderT ?? eval_env)

    let p_succ = Prob.probabilityOf @precT' success mu
    let expected = _EQ @precT eps p_min p_succ -- Expected Cost
    let eps_a = eps / 2
    let num_repetitions = _QryClassicalMax eps_a p_min
    let eps_f = (eps - eps_a) / num_repetitions

    cost <- P.quantumQueryCostE eps_f sigma P.FunCallE{P.fname = sampler, P.args = sampler_args}

    return $ expected Alg..* cost
   where
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

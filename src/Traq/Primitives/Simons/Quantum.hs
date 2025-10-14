{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Simons.Quantum (
  -- * Primitive
  SimonsFindXorPeriod (..),

  -- * Query Formulas
  _SimonsQueries,
) where

import GHC.Generics (Generic)

import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Subtyping

import Traq.Primitives.Simons.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive and Query Cost Formulas
-- ================================================================================

{- |
Simon's Quantum Algorithm with an approximate promise ([1], Theorem 1)

References:

 1. [Breaking Symmetric Cryptosystems using Quantum Period Finding](https://arxiv.org/pdf/1602.05973)
-}
newtype SimonsFindXorPeriod precT = SimonsFindXorPeriod (FindXorPeriod precT)
  deriving (Eq, Show, Read, Generic)

-- | Number of queries as described in Theorem 1.
_SimonsQueries ::
  forall precT.
  (Floating precT) =>
  -- | bitsize
  precT ->
  -- | p_0: maximum probability of spurious collisions for non-period values.
  precT ->
  -- | maximum allowed failure probability.
  precT ->
  precT
_SimonsQueries n p0 eps = q
 where
  -- Sketch:
  -- We need to pick @c@ such that @(2 * ((1 + p0)/2)^c)^n <= eps@ (see Theorem 1)
  -- ==> 2^n / eps <= (2 / (1+p0))^(cn)
  -- define q := cn (i.e. number of queries)
  -- ==> n + log_2 (1/eps) <= q * log_2(2 / (1+p0))

  q_num = n + logBase 2 (1 / eps)
  q_den = logBase 2 (2 / (1 + p0))
  q = q_num / q_den

-- ================================================================================
-- Instances
-- ================================================================================

instance FindXorPeriod precT :<: SimonsFindXorPeriod precT

instance IsA (FindXorPeriod precT) (SimonsFindXorPeriod precT)

instance PP.ToCodeString (SimonsFindXorPeriod Double) where
  build (SimonsFindXorPeriod p) = PP.build p

instance P.CanParsePrimitive (SimonsFindXorPeriod Double) where
  primitiveParser tp = SimonsFindXorPeriod <$> P.primitiveParser tp

instance P.HasFreeVars (SimonsFindXorPeriod precT)
instance
  ( Show precT
  , Ord precT
  , Num precT
  ) =>
  P.TypeCheckablePrimitive (SimonsFindXorPeriod precT)

instance
  ( Integral sizeT
  , Real precT
  , Floating precT
  , Show precT
  , Eq precT
  , Ord precT
  ) =>
  P.UnitaryCostablePrimitive (SimonsFindXorPeriod precT) sizeT precT
  where
  unitaryQueryCostPrimitive delta prim = do
    let FindXorPeriod{fun, p_0, fun_args} = extract prim

    -- size of domain (i.e. @N = 2^n@)
    _N <-
      view
        ( P._funCtx
            . Ctx.at fun
            . non' (error "unable to find function for FindXorPeriod")
        )
        >>= \funDef ->
          case last (P.param_types funDef) of
            P.Fin _N -> return _N
            _ -> error "BUG: FindXorPeriod function has last type wrong, should be caught by typechecker."

    -- compute the bitsize @n@.
    let n :: sizeT
        n = ceiling $ logBase (2 :: Double) $ fromIntegral _N

        n_sym :: precT
        n_sym = P.sizeToPrec n

    -- split
    let delta_alg = delta / 2
    let delta_f_total = delta - delta_alg

    -- compute eps for query cost
    let eps_alg :: precT
        eps_alg = (delta_alg / 2) ^ (2 :: Int)

    -- worst case number of queries
    let qry = _SimonsQueries n_sym p_0 eps_alg

    -- delta per predicate call
    let delta_f = delta_f_total / qry

    cost <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE
          delta_f
          P.FunCallE{P.fname = fun, P.args = fun_args}

    return $ qry Alg..* cost

instance
  ( Integral sizeT
  , Real precT
  , Floating precT
  , Show precT
  , Eq precT
  , Ord precT
  ) =>
  P.QuantumMaxCostablePrimitive (SimonsFindXorPeriod precT) sizeT precT
  where
  quantumMaxQueryCostPrimitive eps prim = do
    let FindXorPeriod{fun, p_0, fun_args} = extract prim

    _N <-
      view
        ( P._funCtx
            . Ctx.at fun
            . non' (error "unable to find function for FindXorPeriod")
        )
        >>= \funDef ->
          case last (P.param_types funDef) of
            P.Fin _N -> return _N
            _ -> error "BUG: FindXorPeriod function has last type wrong, should be caught by typechecker."

    -- compute the bitsize @n@.
    let n :: sizeT
        n = ceiling $ logBase (2 :: Double) $ fromIntegral _N

        n_sym :: precT
        n_sym = P.sizeToPrec n

    -- split
    let eps_alg = eps / 2
    let eps_f_total = eps - eps_alg

    -- worst case number of queries
    let qry = _SimonsQueries n_sym p_0 eps_alg

    -- delta per predicate call
    let eps_f = eps_f_total / qry
    let delta_f = eps_f / 2

    cost <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE
          delta_f
          P.FunCallE{P.fname = fun, P.args = fun_args}

    return $ qry Alg..* cost

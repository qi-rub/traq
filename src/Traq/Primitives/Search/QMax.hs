{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Quantum Max Finding.

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module Traq.Primitives.Search.QMax () where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- [1], Page 16, below Eq. 11
_EQMax :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT
_EQMax n = 6.3505 * sqrt_n + 2.8203
 where
  sqrt_n :: costT
  sqrt_n = sqrt $ fromIntegral n

-- [1], Corollary 1.
_WQMax :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_WQMax n eps = 3 * _EQMax n * log_eps
 where
  log_eps :: costT
  log_eps = log (1 / eps)

-- Worst case cost of unitary QMax.
_WUQMax :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_WUQMax n delta = 2 * _WQMax n eps -- 2x for compute-uncompute
 where
  eps :: costT
  eps = (delta / 2) ^ (2 :: Int)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax = QMax {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Show, Read)

instance PP.ToCodeString QMax where
  build QMax{predicate, pred_args} = PP.putWord $ printf "@max[%s](%s)" predicate (PP.commaList pred_args)

-- Parsing
instance P.CanParsePrimitive QMax where
  primitiveParser tp = do
    symbol tp "@max"
    predicate <- brackets tp $ identifier tp
    pred_args <- parens tp $ commaSep tp $ identifier tp
    return QMax{predicate, pred_args}

-- Type check
instance P.TypeCheckablePrimitive QMax where
  typeCheckPrimitive QMax{predicate, pred_args} = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find `max` predicate `%s`" predicate)

    when (length ret_types /= 1) $
      throwError $
        printf "`max` predicate must return a single value, got %d" (length ret_types)

    arg_tys <- mapM Ctx.lookup pred_args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return ret_types

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance
  (Fractional costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT QMax costT
  where
  evalPrimitive QMax{predicate, pred_args} sigma = do
    pred_fun <- view $ P._funCtx . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.domain

    arg_vals <- runReaderT ?? sigma $ forM pred_args $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")

    vs <- forM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) (P.NamedFunDef predicate pred_fun)
      [P.FinV v] <- pure res
      return v

    return [P.FinV $ maximum vs]

-- ================================================================================
-- Abstract Costs
-- ================================================================================

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT QMax sizeT costT
  where
  unitaryQueryCostPrimitive delta QMax{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _WUQMax n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry * cost_pred

instance
  ( Integral sizeT
  , Floating costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT QMax sizeT costT
  where
  quantumMaxQueryCostPrimitive eps QMax{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _WQMax n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

instance
  ( Integral sizeT
  , Floating costT
  , P.EvaluatablePrimitive primsT QMax costT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT QMax sizeT costT
  where
  quantumQueryCostPrimitive eps QMax{predicate} _ = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_prim = eps / 2
    let eps_pred = eps - eps_prim

    -- number of predicate queries
    let qry = _EQMax n
    let w_qry = _WQMax n eps_prim

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / w_qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

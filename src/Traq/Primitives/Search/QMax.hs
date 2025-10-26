{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- [1], Page 16, below Eq. 11
_EQMax :: forall sizeT precT. (Floating precT, P.SizeToPrec sizeT precT) => sizeT -> precT
_EQMax n = 6.3505 * sqrt_n + 2.8203
 where
  sqrt_n :: precT
  sqrt_n = sqrt $ P.sizeToPrec n

-- [1], Corollary 1.
_WQMax :: forall sizeT precT. (Floating precT, P.SizeToPrec sizeT precT) => sizeT -> P.FailProb precT -> precT
_WQMax n eps = 3 * _EQMax n * log_eps
 where
  log_eps :: precT
  log_eps = log (1 / P.getFailProb eps)

-- Worst case cost of unitary QMax.
_WUQMax :: forall sizeT precT. (Floating precT, P.SizeToPrec sizeT precT) => sizeT -> P.L2NormError precT -> precT
_WUQMax n delta = 2 * _WQMax n eps -- 2x for compute-uncompute
 where
  eps :: P.FailProb precT
  eps = P.requiredNormErrorToFailProb delta

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax sizeT precT = QMax {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Show, Read)

type instance SizeType (QMax sizeT precT) = sizeT
type instance PrecType (QMax sizeT precT) = precT

instance PP.ToCodeString (QMax sizeT precT) where
  build QMax{predicate, pred_args} = PP.putWord $ printf "@max[%s](%s)" predicate (PP.commaList pred_args)

-- Parsing
instance P.Parseable (QMax sizeT precT) where
  parseE tp = do
    symbol tp "@max"
    predicate <- brackets tp $ identifier tp
    pred_args <- parens tp $ commaSep tp $ identifier tp
    return QMax{predicate, pred_args}

-- Type check
instance (P.TypeCheckable sizeT) => P.TypeCheckablePrimitive (QMax sizeT precT) sizeT where
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
instance (P.EvalReqs sizeT precT) => P.Evaluatable (QMax sizeT precT) sizeT precT where
  eval QMax{predicate, pred_args} sigma = do
    pred_fun <- view $ P._funCtx . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.domain

    arg_vals <- runReaderT ?? sigma $ forM pred_args $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")

    vs <- forM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) (P.NamedFunDef predicate pred_fun)
      case res of
        [P.FinV v] -> return v
        _ -> error "fail"

    return [P.FinV $ maximum vs]

-- ================================================================================
-- Abstract Costs
-- ================================================================================

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , P.TypeCheckable sizeT
  ) =>
  P.UnitaryCostablePrimitive (QMax sizeT precT) sizeT precT
  where
  unitaryQueryCostPrimitive delta QMax{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the precision
    let delta_search = delta `P.divideError` 2
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _WUQMax n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred `P.divideError` qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry Alg..* cost_pred

instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , P.TypeCheckable sizeT
  ) =>
  P.QuantumMaxCostablePrimitive (QMax sizeT precT) sizeT precT
  where
  quantumMaxQueryCostPrimitive eps QMax{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_search = eps `P.divideError` 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _WQMax n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred `P.divideError` qry
    let delta_per_pred_call = P.requiredFailProbToNormError eps_per_pred_call

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry Alg..* cost_unitary_pred

instance
  ( Integral sizeT
  , Floating precT
  , sizeT ~ SizeT
  , P.SizeToPrec SizeT precT
  , Show precT
  , P.EvalReqs sizeT precT
  ) =>
  P.QuantumCostablePrimitive (QMax sizeT precT) sizeT precT
  where
  quantumQueryCostPrimitive eps QMax{predicate} _ = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    let eps_prim = eps `P.divideError` 2
    let eps_pred = eps - eps_prim

    -- number of predicate queries
    let qry = _EQMax n :: precT
    let w_qry = _WQMax n eps_prim

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred `P.divideError` w_qry
    let delta_per_pred_call = P.requiredFailProbToNormError eps_per_pred_call

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fname = predicate, P.args = undefined}

    return $ qry Alg..* cost_unitary_pred

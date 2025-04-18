{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives.Search.QSearchCFNW (
  _EQSearch,
  _EQSearchWorst,
  _QSearchZalka,
  QSearchCFNW (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (anyM)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- Eq. TODO REF
_EQSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_EQSearchWorst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

-- Eq. TODO REF
_F :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT
_F n t
  | 4 * t < n = 2.0344
  | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

-- Eq. TODO REF
_EQSearch :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT -> costT
_EQSearch n t eps
  | t == 0 = _EQSearchWorst n eps
  | otherwise = _F n t * (1 + 1 / (1 - term))
 where
  term = _F n t / (9.2 * sqrt (fromIntegral n))

-- Eq. TODO REF
_QSearchZalka :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_QSearchZalka n delta = 2 * nq -- for compute-uncompute
 where
  -- fail prob
  eps :: costT
  eps = (delta / 2) ^ (2 :: Int)

  -- log_fac = ceiling log_fac
  log_fac :: costT
  log_fac = log (1 / eps) / (2 * log (4 / 3))

  -- number of queries of the original algorithm.
  nq :: costT
  nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

newtype QSearchCFNW = QSearchCFNW {predicate :: Ident}

instance ToCodeString QSearchCFNW where
  toCodeString QSearchCFNW{predicate} = printf "any[%s]" predicate

-- | TypeCheck an `any` call
instance P.TypeCheckablePrimitive QSearchCFNW sizeT where
  typeCheckPrimitive QSearchCFNW{predicate} args = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return [P.tbool]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance P.EvaluatablePrimitive QSearchCFNW where
  evalPrimitive QSearchCFNW{predicate} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    has_sol <- flip anyM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    return [P.boolToValue has_sol]

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  (Integral sizeT, Floating costT) =>
  P.UnitaryCostablePrimitive QSearchCFNW sizeT costT
  where
  unitaryQueryCostPrimitive delta QSearchCFNW{predicate} = do
    P.FunDef{P.param_types} <- view $ _2 . Ctx.at predicate . singular _Just
    let P.Fin n = param_types & last

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _QSearchZalka n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_pred

instance (Integral sizeT, Floating costT) => UQPL.Lowerable QSearchCFNW sizeT costT

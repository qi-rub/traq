{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives.Search.QSearchCFNW (
  _EQSearch,
  _EQSearchWorst,
  _QSearchZalka,
  QSearchCFNW (..),
  qAnyCFNW,
) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (anyM)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

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

data QSearchCFNW = QSearchCFNW {predicate :: Ident, return_sol :: Bool}
  deriving (Eq, Show, Read)

qAnyCFNW :: Ident -> QSearchCFNW
qAnyCFNW p = QSearchCFNW{return_sol = False, predicate = p}

instance ToCodeString QSearchCFNW where
  toCodeString QSearchCFNW{predicate, return_sol = False} = printf "any[%s]" predicate
  toCodeString QSearchCFNW{predicate, return_sol = True} = printf "search[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QSearchCFNW where
  primitiveParser tp = parseAny <|> parseSearch
   where
    parseAny = do
      reserved tp "any"
      predicate <- brackets tp $ identifier tp
      return QSearchCFNW{predicate, return_sol = False}
    parseSearch = do
      reserved tp "search"
      predicate <- brackets tp $ identifier tp
      return QSearchCFNW{predicate, return_sol = True}

-- | TypeCheck an `any` call
instance P.TypeCheckablePrimitive QSearchCFNW sizeT where
  typeCheckPrimitive QSearchCFNW{predicate, return_sol} args = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return $ P.tbool : [last param_types | return_sol]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT QSearchCFNW
  where
  evalPrimitive QSearchCFNW{predicate, return_sol = False} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    has_sol <- flip anyM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    return [P.boolToValue has_sol]
  evalPrimitive QSearchCFNW{predicate, return_sol = True} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    sols <- flip filterM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    let has_sol = not $ null sols
    let out_vals = if has_sol then sols else search_range
    lift $ Tree.choice [pure [P.boolToValue has_sol, v] | v <- out_vals]

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  unitaryQueryCostPrimitive delta QSearchCFNW{predicate} = do
    P.FunDef{P.param_types} <- view $ _2 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

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

instance
  ( Integral sizeT
  , Floating costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  quantumMaxQueryCostPrimitive eps QSearchCFNW{predicate} = do
    P.FunDef{P.param_types} <- view $ _2 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _EQSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

instance
  ( Integral sizeT
  , Floating costT
  , P.EvaluatablePrimitive primsT QSearchCFNW
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  quantumQueryCostPrimitive eps QSearchCFNW{predicate} vs = do
    predDef@P.FunDef{P.param_types} <- view $ _2 . Ctx.at predicate . singular _Just
    let typ_x = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of solutions
    let space = P.range typ_x
    sols <- do
      env <- (,) <$> view _2 <*> view _3
      -- TODO this is too convoluted...
      return $
        (`runMyReaderT` env) $
          (`filterM` space)
            ( \v -> do
                result <- P.evalFun (vs ++ [v]) predDef
                let [b] = result
                return $ b /= 0
            )

    let n = length space
    let t = minimum $ fmap length sols

    -- number of predicate queries
    let qry = _EQSearch n t eps_search

    let q_worst = _EQSearchWorst n eps_search
    let eps_per_pred_call = eps_pred / q_worst
    let delta_per_pred_call = eps_per_pred_call / 2

    pred_unitary_cost <-
      magnify P.extractUEnv $
        P.unitaryQueryCostE delta_per_pred_call P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}
    return $ qry * pred_unitary_cost

instance
  ( Integral sizeT
  , Floating costT
  , UQPL.Lowerable primsT primsT sizeT costT
  ) =>
  UQPL.Lowerable primsT QSearchCFNW sizeT costT
  where
  lowerPrimitive = error "TODO"

instance
  ( Integral sizeT
  , Floating costT
  , CQPL.Lowerable primsT primsT sizeT costT
  ) =>
  CQPL.Lowerable primsT QSearchCFNW sizeT costT
  where
  lowerPrimitive = error "TODO"

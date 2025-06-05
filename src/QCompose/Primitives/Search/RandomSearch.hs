{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, forM, replicateM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (anyM)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (censor, listen)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec (try)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import Data.Foldable (Foldable (toList))
import QCompose.Primitives.Search.Prelude

-- ================================================================================
-- Cost Formulas
-- ================================================================================

_ERandomSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_ERandomSearchWorst n _ = 2 * n

{- | Primitive implementing search using classical random sampling.
 The unitary mode does a brute-force loop.
-}
newtype RandomSearch = RandomSearch {predicate :: Ident}
  deriving (Eq, Show, Read)

instance ToCodeString RandomSearch where
  toCodeString RandomSearch{predicate} = printf "@any[%s]" predicate

instance P.CanParsePrimitive RandomSearch where
  primitiveParser tp = do
    symbol tp "@any"
    predicate <- brackets tp $ identifier tp
    return RandomSearch{predicate}

instance P.TypeCheckablePrimitive RandomSearch sizeT where
  typeCheckPrimitive RandomSearch{predicate} args = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return [P.tbool]

instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT RandomSearch
  where
  evalPrimitive RandomSearch{predicate} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    has_sol <- flip anyM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    return [P.boolToValue has_sol]

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
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

    -- number of predicate queries
    let qry = n

    -- precision per predicate call
    let delta_per_pred_call = delta / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

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
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _ERandomSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry

    -- cost of each predicate call
    cost_unitary_pred <-
      P.quantumMaxQueryCostE eps_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Traq.Primitives.Search.RandomSearch (
  RandomSearch (..),
) where

import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Primitives.Search.Prelude

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- | Number of predicate queries to unitarily implement random search.
_URandomSearch :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT
{-# HLINT ignore "Eta reduce" #-}
_URandomSearch n = fromIntegral n

-- | Worst case number of predicate queries to implement random search.
_ERandomSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_ERandomSearchWorst n _ = fromIntegral $ 2 * n

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

{- | Primitive implementing search using classical random sampling.
 The unitary mode does a brute-force loop.
-}
newtype RandomSearch = RandomSearch {predicate :: Ident}
  deriving (Eq, Show, Read)

instance HasPrimAny RandomSearch where
  mkAny = RandomSearch
  getPredicateOfAny = predicate

instance PP.ToCodeString RandomSearch where
  build RandomSearch{predicate} = PP.putWord $ printf "@any[%s]" predicate

instance P.CanParsePrimitive RandomSearch where
  primitiveParser = parsePrimAny "any"

instance P.TypeCheckablePrimitive RandomSearch sizeT where
  typeCheckPrimitive = typeCheckPrimAny

instance (P.EvaluatablePrimitive primsT primsT) => P.EvaluatablePrimitive primsT RandomSearch where
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
  P.UnitaryCostablePrimitive primsT RandomSearch sizeT costT
  where
  unitaryQueryCostPrimitive delta RandomSearch{predicate} _ = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

    -- number of predicate queries
    let qry = _URandomSearch n

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
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
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

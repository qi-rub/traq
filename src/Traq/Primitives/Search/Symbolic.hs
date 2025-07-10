{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Search.Symbolic (
  -- * Search Primitive supporting symbolic cost
  QSearchSym (..),

  -- * Formulas
  _QryU,
  _QryQmax,
) where

import Control.Applicative ((<|>))
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Parsec (try)
import Text.Printf (printf)
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Primitives.Search.Prelude

-- ================================================================================
-- Cost Formulas
-- ================================================================================

_QryU :: forall sizeT costT. (Show sizeT, Show costT) => Sym.Sym sizeT -> Sym.Sym costT -> Sym.Sym costT
_QryU n eps = Sym.var $ printf "QryU(%s, %s)" (show n) (show eps)

_QryQmax :: forall sizeT costT. (Show sizeT, Show costT) => Sym.Sym sizeT -> Sym.Sym costT -> Sym.Sym costT
_QryQmax n eps = Sym.var $ printf "QryQmax(%s, %s)" (show n) (show eps)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================
data QSearchSym = QSearchSym Ident | QAnySym Ident
  deriving (Eq, Show, Read)

_predicate :: Lens' QSearchSym Ident
_predicate focus (QSearchSym p) = QSearchSym <$> focus p
_predicate focus (QAnySym p) = QAnySym <$> focus p

instance HasPrimAny QSearchSym where
  mkAny = QSearchSym
  getPredicateOfAny = view _predicate

instance HasPrimSearch QSearchSym where
  mkSearch = QSearchSym
  getPredicateOfSearch = view _predicate

-- Printing
instance PP.ToCodeString QSearchSym where
  build (QAnySym predicate) = PP.putWord $ printf "@any[%s]" predicate
  build (QSearchSym predicate) = PP.putWord $ printf "@search[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QSearchSym where
  primitiveParser tp = try (parsePrimAny "any" tp) <|> try (parsePrimSearch "search" tp)

-- Type check
instance P.TypeCheckablePrimitive QSearchSym sizeT where
  typeCheckPrimitive prim@(QAnySym _) = typeCheckPrimAny prim
  typeCheckPrimitive prim@(QSearchSym _) = typeCheckPrimSearch prim

-- ================================================================================
-- Abstract Costs (worst case)
-- ================================================================================
instance
  ( Show sizeT
  , Num sizeT
  , Eq sizeT
  , Show costT
  , Eq costT
  , Floating costT
  , P.UnitaryCostablePrimitive primsT primsT (Sym.Sym sizeT) (Sym.Sym costT)
  ) =>
  P.UnitaryCostablePrimitive primsT QSearchSym (Sym.Sym sizeT) (Sym.Sym costT)
  where
  unitaryQueryCostPrimitive delta prim _ = do
    fun_def@P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at (prim ^. _predicate) . singular _Just

    let P.Fin n = last param_types

    -- split the precision
    delta_search <-
      view P._precSplitStrat >>= \case
        P.SplitSimple -> return $ delta / 2
        P.SplitUsingNeedsEps -> magnify P._funCtx $ do
          P.needsEps fun_def >>= \case
            True -> return $ delta / 2
            False -> return delta
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _QryU n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall (prim ^. _predicate), P.args = undefined}

    return $ qry * cost_pred

instance
  ( Show sizeT
  , Num sizeT
  , Eq sizeT
  , Show costT
  , Eq costT
  , Floating costT
  , P.QuantumMaxCostablePrimitive primsT primsT (Sym.Sym sizeT) (Sym.Sym costT)
  ) =>
  P.QuantumMaxCostablePrimitive primsT QSearchSym (Sym.Sym sizeT) (Sym.Sym costT)
  where
  quantumMaxQueryCostPrimitive eps prim = do
    fun_def@P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at (prim ^. _predicate) . singular _Just
    let P.Fin n = last param_types

    -- split the fail prob
    eps_search <-
      view P._precSplitStrat >>= \case
        P.SplitSimple -> return $ eps / 2
        P.SplitUsingNeedsEps -> magnify P._funCtx $ do
          P.needsEps fun_def >>= \case
            True -> return $ eps / 2
            False -> return eps
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _QryQmax n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fun_kind = P.FunctionCall (prim ^. _predicate), P.args = undefined}

    return $ qry * cost_unitary_pred

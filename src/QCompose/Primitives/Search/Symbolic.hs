{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives.Search.Symbolic (
  -- * Search Primitive supporting symbolic cost
  QSearchSym (..),

  -- * Formulas
  _QryU,
  _QryQmax,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Lens.Micro
import Lens.Micro.Mtl
import qualified QCompose.Data.Symbolic as Sym
import Text.Parsec (try)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import QCompose.Control.Monad (maybeWithError)

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

import QCompose.Primitives.Search.Prelude

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
data QSearchSym = QSearchSym {predicate :: Ident} | QAnySym {predicate :: Ident}
  deriving (Eq, Show, Read)

instance HasSearch QSearchSym where
  mkAny = QAnySym
  mkSearch = QSearchSym

  getPredicate = predicate
  returnsSol QSearchSym{} = True
  returnsSol _ = False

-- Printing
instance ToCodeString QSearchSym where
  toCodeString QAnySym{predicate} = printf "@any[%s]" predicate
  toCodeString QSearchSym{predicate} = printf "@search[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QSearchSym where
  primitiveParser tp = try parseAny <|> try parseSearch
   where
    parseAny = do
      symbol tp "@any"
      predicate <- brackets tp $ identifier tp
      return QAnySym{predicate}
    parseSearch = do
      symbol tp "@search"
      predicate <- brackets tp $ identifier tp
      return QSearchSym{predicate}

-- Type check
instance P.TypeCheckablePrimitive QSearchSym sizeT where
  typeCheckPrimitive prim args = do
    let predicate = getPredicate prim
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return $ P.tbool : [last param_types | returnsSol prim]

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
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
  unitaryQueryCostPrimitive delta prim = do
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at (predicate prim) . singular _Just
    let P.Fin n = last param_types

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _QryU n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall (predicate prim), P.args = undefined}

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
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at (predicate prim) . singular _Just
    let P.Fin n = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _QryQmax n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall (predicate prim), P.args = undefined}

    return $ qry * cost_unitary_pred

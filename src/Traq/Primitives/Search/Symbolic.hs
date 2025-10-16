{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.Symbolic (
  -- * Search Primitive supporting symbolic cost
  QSearchSym (..),

  -- * Formulas
  _QryU,
  _QryQmax,
) where

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Text.Parsec (try)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Context as Ctx
import Traq.Data.Subtyping
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

_QryU :: forall sizeT precT. (Show sizeT, Show precT) => Sym.Sym sizeT -> P.L2NormError (Sym.Sym precT) -> Sym.Sym precT
_QryU n delta = Sym.var $ printf "QryU(%s, %s)" (show n) (show $ P.getL2NormError delta)

_QryQmax :: forall sizeT precT. (Show sizeT, Show precT) => Sym.Sym sizeT -> P.FailProb (Sym.Sym precT) -> Sym.Sym precT
_QryQmax n eps = Sym.var $ printf "QryQmax(%s, %s)" (show n) (show $ P.getFailProb eps)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================
data QSearchSym = QSearchSym PrimSearch | QAnySym PrimAny
  deriving (Eq, Show, Read, Generic)

_QSearchSym :: Traversal' QSearchSym PrimSearch
_QSearchSym focus (QSearchSym p) = QSearchSym <$> focus p
_QSearchSym _ q = pure q

_QAnySym :: Traversal' QSearchSym PrimAny
_QAnySym focus (QAnySym p) = QAnySym <$> focus p
_QAnySym _ q = pure q

getPred :: QSearchSym -> Ident
getPred (QSearchSym PrimSearch{predicate}) = predicate
getPred (QAnySym PrimAny{predicate}) = predicate

instance PrimAny :<: QSearchSym where
  inject = QAnySym

  project (QAnySym p) = Just p
  project _ = Nothing

instance PrimSearch :<: QSearchSym where
  inject = QSearchSym

  project (QSearchSym p) = Just p
  project _ = Nothing

instance IsA SearchLikePrim QSearchSym

-- Printing
instance PP.ToCodeString QSearchSym where
  build (QAnySym p) = printSearchLikePrim "any" p
  build (QSearchSym p) = printSearchLikePrim "search" p

-- Parsing
instance P.CanParsePrimitive QSearchSym where
  primitiveParser tp =
    try (QAnySym <$> parsePrimAnyWithName "any" tp)
      <|> try (QSearchSym <$> parsePrimSearchWithName "search" tp)

-- Type check
instance P.TypeCheckablePrimitive QSearchSym

-- ================================================================================
-- Abstract Costs (worst case)
-- ================================================================================
instance
  ( Show sizeT
  , Num sizeT
  , Eq sizeT
  , Show precT
  , Eq precT
  , Floating precT
  ) =>
  P.UnitaryCostablePrimitive QSearchSym (Sym.Sym sizeT) (Sym.Sym precT)
  where
  unitaryQueryCostPrimitive delta prim = do
    Just fun_def@P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at (getPred prim)

    P.Fin n <- pure $ last param_types

    -- split the precision
    delta_search <-
      view P._precSplitStrat >>= \case
        P.SplitSimple -> return $ delta `P.divideError` 2
        P.SplitUsingNeedsEps -> magnify P._funCtx $ do
          P.needsEps fun_def >>= \case
            True -> return $ delta `P.divideError` 2
            False -> return delta
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _QryU n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred `P.divideError` qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fname = getPred prim, P.args = undefined}

    return $ qry Alg..* cost_pred

instance
  ( Show sizeT
  , Num sizeT
  , Eq sizeT
  , Show precT
  , Eq precT
  , Floating precT
  ) =>
  P.QuantumMaxCostablePrimitive QSearchSym (Sym.Sym sizeT) (Sym.Sym precT)
  where
  quantumMaxQueryCostPrimitive eps prim = do
    Just fun_def@P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at (getPred prim)
    P.Fin n <- pure $ last param_types

    -- split the fail prob
    eps_search <-
      view P._precSplitStrat >>= \case
        P.SplitSimple -> return $ eps `P.divideError` 2
        P.SplitUsingNeedsEps -> magnify P._funCtx $ do
          P.needsEps fun_def >>= \case
            True -> return $ eps `P.divideError` 2
            False -> return eps
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _QryQmax n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred `P.divideError` qry
    let delta_per_pred_call = P.requiredFailProbToNormError eps_per_pred_call

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fname = getPred prim, P.args = undefined}

    return $ qry Alg..* cost_unitary_pred

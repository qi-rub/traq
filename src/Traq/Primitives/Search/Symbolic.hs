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

import GHC.Generics (Generic)
import Text.Printf (printf)

import qualified Numeric.Algebra as Alg

import Traq.Data.Subtyping
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as P
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================
newtype QSearchSym size prec = QSearchSym (PrimSearch (Sym.Sym size) (Sym.Sym prec))
  deriving (Eq, Show, Generic)

type instance SizeType (QSearchSym size prec) = Sym.Sym size
type instance PrecType (QSearchSym size prec) = Sym.Sym prec

type instance PrimFnShape (QSearchSym size prec) = BooleanPredicate

instance PrimSearch (Sym.Sym size) (Sym.Sym prec) :<: QSearchSym size prec where
  inject = QSearchSym
  project (QSearchSym p) = Just p

instance (Show size) => SerializePrim (QSearchSym size prec) where
  primNames = ["any_sym", "search_sym"]

  parsePrimParams tp "any_sym" = QSearchSym <$> parsePrimParams tp "any"
  parsePrimParams tp "search_sym" = QSearchSym <$> parsePrimParams tp "search"
  parsePrimParams _ _ = fail ""

  printPrimParams (QSearchSym prim) = printPrimParams prim

-- Type check
instance (P.TypingReqs size) => TypeCheckPrim (QSearchSym size prec) (Sym.Sym size) where
  inferRetTypesPrim (QSearchSym p) = inferRetTypesPrim p

-- ================================================================================
-- Abstract Costs (worst case)
-- ================================================================================

getSearchType :: QSearchSym size prec -> P.VarType (Sym.Sym size)
getSearchType (QSearchSym (PrimSearch _ ty)) = ty

_QryU :: forall size prec. (Show size, Show prec) => Sym.Sym size -> P.FailProb (Sym.Sym prec) -> Sym.Sym prec
_QryU n delta = Sym.var $ printf "QryU(%s, %s)" (show n) (show $ P.getFailProb delta)

_QryQmax :: forall size prec. (Show size, Show prec) => Sym.Sym size -> P.FailProb (Sym.Sym prec) -> Sym.Sym prec
_QryQmax n eps = Sym.var $ printf "QryQmax(%s, %s)" (show n) (show $ P.getFailProb eps)

domainSizeSym :: (Num size, Eq size, Show size) => P.VarType (Sym.Sym size) -> Sym.Sym size
domainSizeSym (P.Fin _N) = _N
domainSizeSym (P.Bitvec n) = Sym.var $ printf "(2^(%s))" (show n)
domainSizeSym (P.Arr n t) = n * domainSizeSym t
domainSizeSym (P.Tup ts) = product $ map domainSizeSym ts

instance
  (P.TypingReqs size, Show prec, Num prec, Eq prec) =>
  UnitaryCostPrim (QSearchSym size prec) (Sym.Sym size) (Sym.Sym prec)
  where
  unitaryQueryCosts prim eps = BooleanPredicate $ strongQueries $ _QryU _N eps
   where
    _N = domainSizeSym $ getSearchType prim

  unitaryExprCosts _ _ = Alg.zero

instance
  (P.TypingReqs size, Show prec, Num prec, Eq prec) =>
  QuantumHavocCostPrim (QSearchSym size prec) (Sym.Sym size) (Sym.Sym prec)
  where
  quantumQueryCostsUnitary prim eps = BooleanPredicate $ strongQueries $ _QryQmax _N eps
   where
    _N = domainSizeSym $ getSearchType prim

  quantumQueryCostsQuantum _ _ = BooleanPredicate 0

  quantumExprCosts = Alg.zero

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
newtype QSearchSym sizeT precT = QSearchSym (PrimSearch (Sym.Sym sizeT) (Sym.Sym precT))
  deriving (Eq, Show, Generic)

type instance SizeType (QSearchSym sizeT precT) = Sym.Sym sizeT
type instance PrecType (QSearchSym sizeT precT) = Sym.Sym precT

type instance PrimFnShape (QSearchSym size prec) = BooleanPredicate

instance PrimSearch (Sym.Sym sizeT) (Sym.Sym precT) :<: QSearchSym sizeT precT where
  inject = QSearchSym
  project (QSearchSym p) = Just p

instance (Show sizeT) => SerializePrim (QSearchSym sizeT precT) where
  primNames = ["any_sym", "search_sym"]

  parsePrimParams tp "any_sym" = QSearchSym <$> parsePrimParams tp "any"
  parsePrimParams tp "search_sym" = QSearchSym <$> parsePrimParams tp "search"
  parsePrimParams _ _ = fail ""

  printPrimParams (QSearchSym prim) = printPrimParams prim

-- Type check
instance (P.TypingReqs sizeT) => TypeCheckPrim (QSearchSym sizeT precT) (Sym.Sym sizeT) where
  inferRetTypesPrim (QSearchSym p) = inferRetTypesPrim p

-- ================================================================================
-- Abstract Costs (worst case)
-- ================================================================================

getSearchType :: QSearchSym size prec -> P.VarType (Sym.Sym size)
getSearchType (QSearchSym (PrimSearch _ ty)) = ty

_QryU :: forall sizeT precT. (Show sizeT, Show precT) => Sym.Sym sizeT -> P.L2NormError (Sym.Sym precT) -> Sym.Sym precT
_QryU n delta = Sym.var $ printf "QryU(%s, %s)" (show n) (show $ P.getL2NormError delta)

_QryQmax :: forall sizeT precT. (Show sizeT, Show precT) => Sym.Sym sizeT -> P.FailProb (Sym.Sym precT) -> Sym.Sym precT
_QryQmax n eps = Sym.var $ printf "QryQmax(%s, %s)" (show n) (show $ P.getFailProb eps)

domainSizeSym :: (Num size, Eq size, Show size) => P.VarType (Sym.Sym size) -> Sym.Sym size
domainSizeSym (P.Fin _N) = _N
domainSizeSym (P.Bitvec n) = Sym.var $ printf "(2^(%s))" (show n)
domainSizeSym (P.Arr n t) = n * domainSizeSym t
domainSizeSym (P.Tup ts) = product $ map domainSizeSym ts

instance
  (Eq sizeT, Num sizeT, Show sizeT, Show precT, Num precT, Eq precT) =>
  UnitaryCostPrim (QSearchSym sizeT precT) (Sym.Sym sizeT) (Sym.Sym precT)
  where
  unitaryQueryCosts prim eps = BooleanPredicate $ strongQueries $ _QryQmax _N eps
   where
    _N = domainSizeSym $ getSearchType prim

  unitaryExprCosts _ _ = Alg.zero

instance
  (Eq sizeT, Num sizeT, Num precT, Show sizeT, Show precT, Num precT, Eq precT) =>
  QuantumHavocCostPrim (QSearchSym sizeT precT) (Sym.Sym sizeT) (Sym.Sym precT)
  where
  quantumQueryCostsUnitary prim eps = BooleanPredicate $ strongQueries $ _QryQmax _N eps
   where
    _N = domainSizeSym $ getSearchType prim

  quantumQueryCostsQuantum _ _ = BooleanPredicate 0

  quantumExprCosts = Alg.zero

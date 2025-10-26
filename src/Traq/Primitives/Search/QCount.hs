{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Quantum Counting

Reference: https://arxiv.org/pdf/quant-ph/9805082
-}
module Traq.Primitives.Search.QCount () where

import Text.Printf (printf)

import Traq.Prelude
import Traq.Primitives.Prelude
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QCount sizeT precT = QCount {predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

type instance SizeType (QCount sizeT precT) = sizeT
type instance PrecType (QCount sizeT precT) = precT

instance PP.ToCodeString (QCount sizeT precT) where
  build QCount{predicate, args} = PP.putWord $ printf "@count[%s](%s)" predicate (PP.commaList args)

-- Parsing
instance P.Parseable (QCount sizeT precT) where
  parseE tp = do
    [(predicate, args)] <- parsePrimWithPredicates "count" 1 tp
    return QCount{predicate, args}

-- Type check
instance (P.TypeCheckable sizeT) => P.TypeCheckablePrimitive (QCount sizeT precT) sizeT where
  typeCheckPrimitive QCount{predicate, args} = do
    s_tys <- typeCheckSearchPredicate predicate args
    let n_items = product $ map P.domainSize s_tys
    return [P.Fin (n_items + 1)]

instance (P.EvalReqs sizeT precT) => P.Evaluatable (QCount sizeT precT) sizeT precT where
  eval QCount{predicate, args} = evaluatePrimCount predicate args

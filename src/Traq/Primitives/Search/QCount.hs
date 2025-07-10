{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Quantum Counting

Reference: https://arxiv.org/pdf/quant-ph/9805082
-}
module Traq.Primitives.Search.QCount () where

import Text.Printf (printf)

import Traq.Prelude
import Traq.Primitives.Search.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

newtype QCount = QCount {predicate :: Ident}
  deriving (Eq, Show, Read)

instance PP.ToCodeString QCount where
  build QCount{predicate} = PP.putWord $ printf "@count[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QCount where
  primitiveParser tp = do
    [predicate] <- parsePrimWithPredicates "count" 1 tp
    return QCount{predicate}

-- Type check
instance P.TypeCheckablePrimitive QCount sizeT where
  typeCheckPrimitive QCount{predicate} args = do
    s_tys <- typeCheckSearchPredicate predicate args
    let n_items = product $ map (\(P.Fin n) -> n) s_tys
    return [P.Fin (n_items + 1)]

instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT QCount
  where
  evalPrimitive QCount{predicate} = evaluatePrimCount predicate

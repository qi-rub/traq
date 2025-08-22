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

data QCount = QCount {predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

instance PP.ToCodeString QCount where
  build QCount{predicate, args} = PP.putWord $ printf "@count[%s](%s)" predicate (PP.commaList args)

-- Parsing
instance P.CanParsePrimitive QCount where
  primitiveParser tp = do
    [(predicate, args)] <- parsePrimWithPredicates "count" 1 tp
    return QCount{predicate, args}

-- Type check
instance P.TypeCheckablePrimitive QCount where
  typeCheckPrimitive QCount{predicate, args} = do
    s_tys <- typeCheckSearchPredicate predicate args
    let n_items = product $ map P.domainSize s_tys
    return [P.Fin (n_items + 1)]

instance
  (Fractional costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT QCount costT
  where
  evalPrimitive QCount{predicate, args} = evaluatePrimCount predicate args

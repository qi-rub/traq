{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives (
  DefaultPrims (..),
  QSearchCFNW (..),
) where

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives.Search.QSearchCFNW

newtype DefaultPrims = QAny QSearchCFNW

instance ToCodeString DefaultPrims where
  toCodeString (QAny q) = toCodeString q
  toCodeLines (QAny q) = toCodeLines q

instance P.TypeCheckablePrimitive DefaultPrims sizeT where
  typeCheckPrimitive (QAny q) = P.typeCheckPrimitive q

instance (Integral sizeT, Floating costT) => P.UnitaryCostablePrimitive DefaultPrims sizeT costT where
  unitaryQueryCostPrimitive delta (QAny q) = P.unitaryQueryCostPrimitive delta q

instance (Integral sizeT, Floating costT) => UQPL.Lowerable DefaultPrims sizeT costT where
  lowerPrimitive (QAny q) = UQPL.lowerPrimitive q

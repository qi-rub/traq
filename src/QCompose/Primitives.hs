{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives (
  DefaultPrims (..),
  QSearchCFNW (..),
  qAnyCFNW,
) where

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives.Search.QSearchCFNW

newtype DefaultPrims = QAny QSearchCFNW
  deriving (Eq, Show, Read)

-- Printing
instance ToCodeString DefaultPrims where
  toCodeString (QAny q) = toCodeString q
  toCodeLines (QAny q) = toCodeLines q

-- Parsing
instance P.CanParsePrimitive DefaultPrims where
  primitiveParser tp = QAny <$> P.primitiveParser tp

-- Type Checking
instance P.TypeCheckablePrimitive DefaultPrims sizeT where
  typeCheckPrimitive (QAny q) = P.typeCheckPrimitive q

-- Evaluation
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT DefaultPrims
  where
  evalPrimitive (QAny q) = P.evalPrimitive q

-- Costs
instance
  ( Integral sizeT
  , Floating costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT DefaultPrims sizeT costT
  where
  unitaryQueryCostPrimitive delta (QAny q) = P.unitaryQueryCostPrimitive delta q

instance
  ( Integral sizeT
  , Floating costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT DefaultPrims sizeT costT
  where
  quantumMaxQueryCostPrimitive delta (QAny q) = P.quantumMaxQueryCostPrimitive delta q

instance
  ( Integral sizeT
  , Floating costT
  , P.EvaluatablePrimitive primsT primsT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT DefaultPrims sizeT costT
  where
  quantumQueryCostPrimitive delta (QAny q) = P.quantumQueryCostPrimitive delta q

-- Lowering
instance
  ( Integral sizeT
  , Floating costT
  , UQPL.Lowerable primsT primsT sizeT costT
  ) =>
  UQPL.Lowerable primsT DefaultPrims sizeT costT
  where
  lowerPrimitive (QAny q) = UQPL.lowerPrimitive q

instance
  ( Integral sizeT
  , Floating costT
  , CQPL.Lowerable primsT primsT sizeT costT
  ) =>
  CQPL.Lowerable primsT DefaultPrims sizeT costT
  where
  lowerPrimitive (QAny q) = CQPL.lowerPrimitive q

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives (
  DefaultPrims (..),
  QSearchCFNW (..),
) where

import qualified Traq.Compiler.Quantum as CQPL
import qualified Traq.Compiler.Unitary as UQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch

data DefaultPrims = QAny QSearchCFNW | RAny RandomSearch
  deriving (Eq, Show, Read)

instance HasPrimAny DefaultPrims where
  mkAny = QAny . mkAny

  getPredicateOfAny (QAny q) = getPredicateOfAny q
  getPredicateOfAny _ = error "invalid primitive"

instance HasPrimSearch DefaultPrims where
  mkSearch = QAny . mkSearch

  getPredicateOfSearch (QAny q) = getPredicateOfSearch q
  getPredicateOfSearch _ = error "invalid primitive"

-- Printing
instance PP.ToCodeString DefaultPrims where
  toCodeString (QAny prim) = PP.toCodeString prim
  toCodeString (RAny prim) = PP.toCodeString prim

  toCodeLines (QAny prim) = PP.toCodeLines prim
  toCodeLines (RAny prim) = PP.toCodeLines prim

-- Parsing
instance P.CanParsePrimitive DefaultPrims where
  primitiveParser tp = QAny <$> P.primitiveParser tp

-- Type Checking
instance P.TypeCheckablePrimitive DefaultPrims sizeT where
  typeCheckPrimitive (QAny prim) = P.typeCheckPrimitive prim
  typeCheckPrimitive (RAny prim) = P.typeCheckPrimitive prim

-- Evaluation
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT DefaultPrims
  where
  evalPrimitive (QAny prim) = P.evalPrimitive prim
  evalPrimitive (RAny prim) = P.evalPrimitive prim

-- Costs
instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT DefaultPrims sizeT costT
  where
  unitaryQueryCostPrimitive delta (QAny prim) = P.unitaryQueryCostPrimitive delta prim
  unitaryQueryCostPrimitive delta (RAny prim) = P.unitaryQueryCostPrimitive delta prim

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT DefaultPrims sizeT costT
  where
  quantumMaxQueryCostPrimitive delta (QAny prim) = P.quantumMaxQueryCostPrimitive delta prim
  quantumMaxQueryCostPrimitive delta (RAny prim) = P.quantumMaxQueryCostPrimitive delta prim

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.EvaluatablePrimitive primsT primsT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT DefaultPrims sizeT costT
  where
  quantumQueryCostPrimitive delta (QAny prim) = P.quantumQueryCostPrimitive delta prim

-- Lowering
instance
  ( Integral sizeT
  , Floating costT
  , UQPL.Lowerable primsT primsT holeT sizeT costT
  , UQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  UQPL.Lowerable primsT DefaultPrims holeT sizeT costT
  where
  lowerPrimitive delta (QAny q) = UQPL.lowerPrimitive delta q

instance
  ( Integral sizeT
  , Floating costT
  , CQPL.Lowerable primsT primsT holeT sizeT costT
  , CQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  CQPL.Lowerable primsT DefaultPrims holeT sizeT costT
  where
  lowerPrimitive eps (QAny q) = CQPL.lowerPrimitive eps q

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives (
  DefaultPrims (..),
  QSearchCFNW (..),
) where

import Control.Applicative ((<|>))
import Text.Printf (printf)

import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch

data DefaultPrims
  = QAny QSearchCFNW
  | RAny RandomSearch
  | DAny DetSearch
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
  build (QAny prim) = PP.build prim
  build (RAny RandomSearch{predicate}) = PP.putWord $ printf "@any_rand[%s]" predicate
  build (DAny DetSearch{predicate}) = PP.putWord $ printf "@any_det[%s]" predicate

-- Parsing
instance P.CanParsePrimitive DefaultPrims where
  primitiveParser tp =
    (QAny <$> parsePrimAny "any" tp)
      <|> (RAny <$> parsePrimAny "any_rand" tp)
      <|> (DAny <$> parsePrimAny "any_det" tp)

-- Type Checking
instance P.TypeCheckablePrimitive DefaultPrims sizeT where
  typeCheckPrimitive (QAny prim) = P.typeCheckPrimitive prim
  typeCheckPrimitive (RAny prim) = P.typeCheckPrimitive prim
  typeCheckPrimitive (DAny prim) = P.typeCheckPrimitive prim

-- Evaluation
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT DefaultPrims
  where
  evalPrimitive (QAny prim) = P.evalPrimitive prim
  evalPrimitive (RAny prim) = P.evalPrimitive prim
  evalPrimitive (DAny prim) = P.evalPrimitive prim

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
  unitaryQueryCostPrimitive delta (DAny prim) = P.unitaryQueryCostPrimitive delta prim

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
  quantumMaxQueryCostPrimitive delta (DAny prim) = P.quantumMaxQueryCostPrimitive delta prim

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
  quantumQueryCostPrimitive delta (RAny prim) = P.quantumQueryCostPrimitive delta prim
  quantumQueryCostPrimitive delta (DAny prim) = P.quantumQueryCostPrimitive delta prim

-- Lowering
instance
  ( Integral sizeT
  , Floating costT
  , CompileU.Lowerable primsT primsT holeT sizeT costT
  , CompileU.Lowerable primsT QSearchCFNW holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  CompileU.Lowerable primsT DefaultPrims holeT sizeT costT
  where
  lowerPrimitive delta (QAny q) = CompileU.lowerPrimitive delta q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating costT
  , CompileQ.Lowerable primsT primsT holeT sizeT costT
  , CompileQ.Lowerable primsT QSearchCFNW holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  CompileQ.Lowerable primsT DefaultPrims holeT sizeT costT
  where
  lowerPrimitive eps (QAny q) = CompileQ.lowerPrimitive eps q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

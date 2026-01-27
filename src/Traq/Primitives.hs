{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives (
  -- * Specialized typeclasses
  module Traq.Primitives.Class,

  -- * Collections

  -- ** Default collection (with exp cost + compile)
  DefaultPrimCollection (..),
  DefaultPrims,
  DefaultPrims',

  -- ** primitives with worst-case cost support.
  WorstCasePrims,
  WorstCasePrims',
) where

import GHC.Generics

import qualified Traq.Analysis as A
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Default primitives: Supports exp cost and compile
-- ================================================================================

data DefaultPrimCollection sizeT precT
  = QAny (QSearchCFNW sizeT precT)
  | RAny (RandomSearch sizeT precT)
  | DAny (DetSearch sizeT precT)
  deriving (Eq, Show, Generic)

type instance SizeType (DefaultPrimCollection sizeT precT) = sizeT
type instance PrecType (DefaultPrimCollection sizeT precT) = precT

type instance PrimFnShape (DefaultPrimCollection sizeT precT) = []

instance P.MapSize (DefaultPrimCollection size prec) where
  type MappedSize (DefaultPrimCollection size prec) size' = DefaultPrimCollection size' prec

  mapSize f (QAny p) = QAny (P.mapSize f p)
  mapSize f (RAny p) = RAny (P.mapSize f p)
  mapSize f (DAny p) = DAny (P.mapSize f p)

instance (Show size) => SerializePrim (DefaultPrimCollection size prec) where
  primNames = ["any", "search", "any_rand", "any_det"]

  primNameOf (QAny (QSearchCFNW (PrimSearch AnyK _))) = "any"
  primNameOf (QAny (QSearchCFNW (PrimSearch SearchK _))) = "search"
  primNameOf (RAny (RandomSearch (PrimSearch AnyK _))) = "any_rand"
  primNameOf (DAny (DetSearch (PrimSearch AnyK _))) = "any_det"
  primNameOf _ = error "unsupported"

  parsePrimParams tp s
    | s == "any" || s == "search" = QAny <$> parsePrimParams tp s
    | s == "any_rand" = RAny <$> parsePrimParams tp s
    | s == "any_det" = DAny <$> parsePrimParams tp s
    | otherwise = fail $ "unsupported primitive: " ++ s

  printPrimParams (QAny p) = printPrimParams p
  printPrimParams (RAny p) = printPrimParams p
  printPrimParams (DAny p) = printPrimParams p

-- Generic instances
instance (P.TypingReqs size) => TypeCheckPrim (DefaultPrimCollection size prec) size
instance EvalPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, Floating prec) =>
  UnitaryCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.EvalReqs size prec, Floating prec) =>
  QuantumExpCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (DefaultPrimCollection size prec) size prec

type DefaultPrims sizeT precT = Primitive (DefaultPrimCollection sizeT precT)

type DefaultPrims' = DefaultPrims SizeT Double

instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , sizeT ~ SizeT
  ) =>
  Compiler.CompileQ (A.AnnFailProb (DefaultPrims sizeT precT))
  where
  compileQ (A.AnnFailProb eps (Primitive fs (QAny q))) = Compiler.compileQ (A.AnnFailProb eps (Primitive fs q))
  compileQ _ = error "TODO: lowerPrimitive"

-- ================================================================================
-- Worst-cost prim collection
-- ================================================================================

data WorstCasePrimCollection size prec
  = FromDefault (DefaultPrimCollection size prec)
  | Simon (SimonsFindXorPeriod size prec)
  deriving (Eq, Show, Generic)

type instance SizeType (WorstCasePrimCollection sizeT precT) = sizeT
type instance PrecType (WorstCasePrimCollection sizeT precT) = precT

type instance PrimFnShape (WorstCasePrimCollection sizeT precT) = []

instance P.MapSize (WorstCasePrimCollection size prec) where
  type MappedSize (WorstCasePrimCollection size prec) size' = WorstCasePrimCollection size' prec

  mapSize f (FromDefault p) = FromDefault (P.mapSize f p)
  mapSize f (Simon p) = Simon (P.mapSize f p)

instance (Show size, prec ~ Double) => SerializePrim (WorstCasePrimCollection size prec) where
  primNames = ["any", "search", "any_rand", "any_det", "findXorPeriod"]

  primNameOf (FromDefault p) = primNameOf p
  primNameOf (Simon _) = "findXorPeriod"

  parsePrimParams tp s
    | s == "findXorPeriod" = Simon <$> parsePrimParams tp ""
    | otherwise = FromDefault <$> parsePrimParams tp s

  printPrimParams (FromDefault p) = printPrimParams p
  printPrimParams (Simon p) = printPrimParams p

-- Generic instances
instance
  (P.TypingReqs size, Num prec, Ord prec, Show prec) =>
  TypeCheckPrim (WorstCasePrimCollection size prec) size
instance
  (P.TypingReqs size, Integral size, Floating prec, Ord prec, Show prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (WorstCasePrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, Floating prec, Ord prec, Show prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (WorstCasePrimCollection size prec) size prec

type WorstCasePrims sizeT precT = Primitive (WorstCasePrimCollection sizeT precT)

type WorstCasePrims' = WorstCasePrims SizeT Double

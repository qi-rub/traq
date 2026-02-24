{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

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
import Traq.Prelude
import Traq.Primitives.Amplify.CAmplify
import Traq.Primitives.Amplify.QAmplify
import Traq.Primitives.Class
import Traq.Primitives.Max.QMax
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Default primitives: Supports exp cost and compile
-- ================================================================================

data DefaultPrimCollection size prec
  = QAny (QSearchCFNW size prec)
  | RAny (RandomSearch size prec)
  | DAny (DetSearch size prec)
  | CAmp (CAmplify size prec)
  | QAmp (QAmplify size prec)
  | QMax' (QMax size prec)
  deriving (Eq, Show, Generic)

type instance SizeType (DefaultPrimCollection size prec) = size
type instance PrecType (DefaultPrimCollection size prec) = prec

type instance PrimFnShape (DefaultPrimCollection size prec) = []

instance P.MapSize (DefaultPrimCollection size prec) where
  type MappedSize (DefaultPrimCollection size prec) size' = DefaultPrimCollection size' prec

  mapSize f (QAny p) = QAny (P.mapSize f p)
  mapSize f (RAny p) = RAny (P.mapSize f p)
  mapSize f (DAny p) = DAny (P.mapSize f p)
  mapSize f (CAmp p) = CAmp (P.mapSize f p)
  mapSize f (QAmp p) = QAmp (P.mapSize f p)
  mapSize f (QMax' p) = QMax' (P.mapSize f p)

instance (Show size, Show prec, Fractional prec) => SerializePrim (DefaultPrimCollection size prec) where
  primNames =
    [ "any"
    , "search"
    , "any_rand"
    , "any_det"
    , "amplify"
    , "camplify"
    , "max"
    ]

  primNameOf (QAny (QSearchCFNW (PrimSearch AnyK _))) = "any"
  primNameOf (QAny (QSearchCFNW (PrimSearch SearchK _))) = "search"
  primNameOf (QAny _) = error "unsupported QAny"
  primNameOf (RAny (RandomSearch (PrimSearch AnyK _))) = "any_rand"
  primNameOf (RAny _) = error "unsupported QAny"
  primNameOf (DAny (DetSearch (PrimSearch AnyK _))) = "any_det"
  primNameOf (DAny _) = error "unsupported QAny"
  primNameOf (QAmp _) = "amplify"
  primNameOf (CAmp _) = "camplify"
  primNameOf (QMax' _) = "max"

  parsePrimParams tp s
    | s == "any" || s == "search" = QAny <$> parsePrimParams tp s
    | s == "any_rand" = RAny <$> parsePrimParams tp s
    | s == "any_det" = DAny <$> parsePrimParams tp s
    | s == "amplify" = QAmp <$> parsePrimParams tp s
    | s == "camplify" = CAmp <$> parsePrimParams tp s
    | s == "max" = QMax' <$> parsePrimParams tp s
    | otherwise = fail $ "unsupported primitive: " ++ s

  printPrimParams (QAny p) = printPrimParams p
  printPrimParams (RAny p) = printPrimParams p
  printPrimParams (DAny p) = printPrimParams p
  printPrimParams (CAmp p) = printPrimParams p
  printPrimParams (QAmp p) = printPrimParams p
  printPrimParams (QMax' p) = printPrimParams p

-- Generic instances
instance (P.TypingReqs size) => TypeCheckPrim (DefaultPrimCollection size prec) size
instance (Ord prec) => EvalPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.EvalReqs size prec, Floating prec, Ord prec) =>
  QuantumExpCostPrim (DefaultPrimCollection size prec) size prec
instance
  (P.TypingReqs size, Integral size, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (DefaultPrimCollection size prec) size prec
instance
  (size ~ SizeT, P.TypingReqs size, Integral size, RealFloat prec, Show prec) =>
  QuantumCompilePrim (DefaultPrimCollection size prec) size prec

type DefaultPrims size prec = Primitive (DefaultPrimCollection size prec)

type DefaultPrims' = DefaultPrims SizeT Double

-- ================================================================================
-- Worst-cost prim collection
-- ================================================================================

data WorstCasePrimCollection size prec
  = FromDefault (DefaultPrimCollection size prec)
  | Simon (SimonsFindXorPeriod size prec)
  deriving (Eq, Show, Generic)

type instance SizeType (WorstCasePrimCollection size prec) = size
type instance PrecType (WorstCasePrimCollection size prec) = prec

type instance PrimFnShape (WorstCasePrimCollection size prec) = []

instance P.MapSize (WorstCasePrimCollection size prec) where
  type MappedSize (WorstCasePrimCollection size prec) size' = WorstCasePrimCollection size' prec

  mapSize f (FromDefault p) = FromDefault (P.mapSize f p)
  mapSize f (Simon p) = Simon (P.mapSize f p)

instance (Show size, prec ~ Double) => SerializePrim (WorstCasePrimCollection size prec) where
  primNames = primNames @(DefaultPrimCollection size prec) ++ ["findXorPeriod"]

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
instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (WorstCasePrimCollection size prec) size prec
instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  QuantumCompilePrim (WorstCasePrimCollection size prec) size prec

type WorstCasePrims size prec = Primitive (WorstCasePrimCollection size prec)

type WorstCasePrims' = WorstCasePrims SizeT Double

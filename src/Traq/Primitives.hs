{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives (
  module Traq.Primitives.Class,
  DefaultPrimCollection (..),
  DefaultPrims,
  DefaultPrims',
  QSearchCFNW (..),
) where

import GHC.Generics

import qualified Traq.Analysis as A
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import qualified Traq.ProtoLang as P

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

type DefaultPrims sizeT precT = Primitive (DefaultPrimCollection sizeT precT)

type DefaultPrims' = DefaultPrims SizeT Double

-- Lowering
instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , A.SizeToPrec sizeT precT
  ) =>
  CompileU.Lowerable (A.AnnFailProb (DefaultPrims sizeT precT)) sizeT precT
  where
  lowerPrimitive _ (A.AnnFailProb eps (Primitive fs (QAny q))) = CompileU.lowerPrimitive (error "use annotation") (A.AnnFailProb eps (Primitive fs q))
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , sizeT ~ SizeT
  ) =>
  CompileQ.Lowerable (A.AnnFailProb (DefaultPrims sizeT precT)) sizeT precT
  where
  lowerPrimitive _ (A.AnnFailProb eps (Primitive fs (QAny q))) = CompileQ.lowerPrimitive undefined (A.AnnFailProb eps (Primitive fs q))
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

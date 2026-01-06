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

import Control.Monad.Except (liftEither)
import GHC.Generics

import qualified Traq.Data.Probability as Prob

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

instance (P.TypingReqs sizeT) => TypeCheckPrim (DefaultPrimCollection sizeT precT) sizeT where
  inferRetTypesPrim (QAny p) fs = do
    fs' <- liftEither $ listToShape fs
    inferRetTypesPrim p fs'
  inferRetTypesPrim (RAny p) fs = do
    fs' <- liftEither $ listToShape fs
    inferRetTypesPrim p fs'
  inferRetTypesPrim (DAny p) fs = do
    fs' <- liftEither $ listToShape fs
    inferRetTypesPrim p fs'

-- Evaluation
instance EvalPrim (DefaultPrimCollection sizeT precT) sizeT precT where
  evalPrim (QAny p) fs = evalPrim p (reshapeUnsafe fs)
  evalPrim (RAny p) fs = evalPrim p (reshapeUnsafe fs)
  evalPrim (DAny p) fs = evalPrim p (reshapeUnsafe fs)

-- Costs
instance (P.TypingReqs size, Integral size, Floating prec) => UnitaryCostPrim (DefaultPrimCollection size prec) size prec where
  unitaryQueryCosts (QAny p) = shapeToList . unitaryQueryCosts p
  unitaryQueryCosts (RAny p) = shapeToList . unitaryQueryCosts p
  unitaryQueryCosts (DAny p) = shapeToList . unitaryQueryCosts p

  unitaryExprCosts (QAny p) = unitaryExprCosts p
  unitaryExprCosts (RAny p) = unitaryExprCosts p
  unitaryExprCosts (DAny p) = unitaryExprCosts p

instance
  (P.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (DefaultPrimCollection size prec) size prec
  where
  quantumQueryCostsQuantum (QAny p) = shapeToList . quantumQueryCostsQuantum p
  quantumQueryCostsQuantum (RAny p) = shapeToList . quantumQueryCostsQuantum p
  quantumQueryCostsQuantum (DAny p) = shapeToList . quantumQueryCostsQuantum p

  quantumQueryCostsUnitary (QAny p) = shapeToList . quantumQueryCostsUnitary p
  quantumQueryCostsUnitary (RAny p) = shapeToList . quantumQueryCostsUnitary p
  quantumQueryCostsUnitary (DAny p) = shapeToList . quantumQueryCostsUnitary p

  quantumExprCosts (QAny p) = quantumExprCosts p
  quantumExprCosts (RAny p) = quantumExprCosts p
  quantumExprCosts (DAny p) = quantumExprCosts p

instance
  (size ~ SizeT, P.TypingReqs size, Integral size, Floating prec, A.SizeToPrec size prec, Prob.RVType prec prec) =>
  QuantumExpCostPrim (DefaultPrimCollection size prec) size prec
  where
  quantumExpQueryCostsQuantum (QAny p) eps fs = shapeToList $ quantumExpQueryCostsQuantum p eps (reshapeUnsafe fs)
  quantumExpQueryCostsQuantum (RAny p) eps fs = shapeToList $ quantumExpQueryCostsQuantum p eps (reshapeUnsafe fs)
  quantumExpQueryCostsQuantum (DAny p) eps fs = shapeToList $ quantumExpQueryCostsQuantum p eps (reshapeUnsafe fs)

  quantumExpQueryCostsUnitary (QAny p) eps fs = shapeToList $ quantumExpQueryCostsUnitary p eps (reshapeUnsafe fs)
  quantumExpQueryCostsUnitary (RAny p) eps fs = shapeToList $ quantumExpQueryCostsUnitary p eps (reshapeUnsafe fs)
  quantumExpQueryCostsUnitary (DAny p) eps fs = shapeToList $ quantumExpQueryCostsUnitary p eps (reshapeUnsafe fs)

  quantumExpExprCosts (QAny p) eps fs = quantumExpExprCosts p eps (reshapeUnsafe fs)
  quantumExpExprCosts (RAny p) eps fs = quantumExpExprCosts p eps (reshapeUnsafe fs)
  quantumExpExprCosts (DAny p) eps fs = quantumExpExprCosts p eps (reshapeUnsafe fs)

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
  CompileU.Lowerable (DefaultPrims sizeT precT) sizeT precT
  where
  lowerPrimitive delta (Primitive fs (QAny q)) = CompileU.lowerPrimitive delta (Primitive fs q)
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , sizeT ~ SizeT
  ) =>
  CompileQ.Lowerable (DefaultPrims sizeT precT) sizeT precT
  where
  lowerPrimitive eps (Primitive fs (QAny q)) = CompileQ.lowerPrimitive eps (Primitive fs q)
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

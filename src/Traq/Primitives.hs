{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives (
  DefaultPrims (..),
  QSearchCFNW (..),
) where

import Control.Applicative ((<|>))
import GHC.Generics
import Text.Printf (printf)

import Lens.Micro.GHC

import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

data DefaultPrims
  = QAny QSearchCFNW
  | RAny RandomSearch
  | DAny DetSearch
  deriving (Eq, Show, Read, Generic)
  deriving
    ( P.TypeCheckablePrimitive
    , P.HasFreeVars
    )

_QAny :: Traversal' DefaultPrims QSearchCFNW
_QAny focus (QAny p) = QAny <$> focus p
_QAny _ q = pure q

instance HasPrimAny DefaultPrims where
  _PrimAny = _QAny . _PrimAny
  mkPrimAny a b = QAny $ mkPrimAny a b

instance HasPrimSearch DefaultPrims where
  _PrimSearch = _QAny . _PrimSearch
  mkPrimSearch a b = QAny $ mkPrimSearch a b

-- Printing
instance PP.ToCodeString DefaultPrims where
  build (QAny prim) = PP.build prim
  build (RAny RandomSearch{predicate, args}) = PP.putWord $ printf "@any_rand[%s](%s)" predicate (PP.commaList args)
  build (DAny DetSearch{predicate, args}) = PP.putWord $ printf "@any_det[%s](%s)" predicate (PP.commaList args)

-- Parsing
instance P.CanParsePrimitive DefaultPrims where
  primitiveParser tp =
    (QAny <$> P.primitiveParser tp)
      <|> (RAny <$> parsePrimAny "any_rand" tp)
      <|> (DAny <$> parsePrimAny "any_det" tp)

-- Evaluation
instance
  (Fractional costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT DefaultPrims costT

-- Costs
instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT DefaultPrims sizeT costT

instance
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT DefaultPrims sizeT costT

instance
  ( Floating costT
  , Ord costT
  , sizeT ~ SizeT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumCostablePrimitive primsT DefaultPrims sizeT costT

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

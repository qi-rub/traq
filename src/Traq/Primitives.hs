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
import Text.Parsec (try)

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
  mkPrimAny = QAny . mkPrimAny

instance HasPrimSearch DefaultPrims where
  _PrimSearch = _QAny . _PrimSearch
  mkPrimSearch = QAny . mkPrimSearch

-- Printing
instance PP.ToCodeString DefaultPrims where
  build (QAny (QAnyCFNW p)) = printSearchLikePrim "any" p
  build (QAny (QSearchCFNW p)) = printSearchLikePrim "search" p
  build (RAny p) = printSearchLikePrim "any_rand" p
  build (DAny p) = printSearchLikePrim "any_det" p

-- Parsing
instance P.CanParsePrimitive DefaultPrims where
  primitiveParser tp = try qany <|> try qsearch <|> try rany <|> try dany
   where
    qany = QAny . QAnyCFNW <$> parsePrimAnyWithName "any" tp
    qsearch = QAny . QSearchCFNW <$> parsePrimSearchWithName "search" tp
    rany = RAny . RandomSearch <$> parsePrimAnyWithName "any_rand" tp
    dany = DAny . DetSearch <$> parsePrimAnyWithName "any_det" tp

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
  , CompileU.Lowerable primsT primsT sizeT costT
  , CompileU.Lowerable primsT QSearchCFNW sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  CompileU.Lowerable primsT DefaultPrims sizeT costT
  where
  lowerPrimitive delta (QAny q) = CompileU.lowerPrimitive delta q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating costT
  , CompileQ.Lowerable primsT primsT sizeT costT
  , CompileQ.Lowerable primsT QSearchCFNW sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  ) =>
  CompileQ.Lowerable primsT DefaultPrims sizeT costT
  where
  lowerPrimitive eps (QAny q) = CompileQ.lowerPrimitive eps q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

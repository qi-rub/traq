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

import qualified Traq.Data.Probability as Prob

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
  (Fractional precT, Prob.ProbType precT, P.EvaluatablePrimitive primsT primsT precT) =>
  P.EvaluatablePrimitive primsT DefaultPrims precT

-- Costs
instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , P.UnitaryCostablePrimitive primsT primsT sizeT precT
  ) =>
  P.UnitaryCostablePrimitive primsT DefaultPrims sizeT precT

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT precT
  ) =>
  P.QuantumMaxCostablePrimitive primsT DefaultPrims sizeT precT

instance
  ( Floating precT
  , Ord precT
  , Prob.ProbType precT
  , sizeT ~ SizeT
  , P.QuantumCostablePrimitive primsT primsT sizeT precT
  ) =>
  P.QuantumCostablePrimitive primsT DefaultPrims sizeT precT

-- Lowering
instance
  ( Integral sizeT
  , Floating precT
  , CompileU.Lowerable primsT primsT sizeT precT
  , CompileU.Lowerable primsT QSearchCFNW sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  ) =>
  CompileU.Lowerable primsT DefaultPrims sizeT precT
  where
  lowerPrimitive delta (QAny q) = CompileU.lowerPrimitive delta q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating precT
  , CompileQ.Lowerable primsT primsT sizeT precT
  , CompileQ.Lowerable primsT QSearchCFNW sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  ) =>
  CompileQ.Lowerable primsT DefaultPrims sizeT precT
  where
  lowerPrimitive eps (QAny q) = CompileQ.lowerPrimitive eps q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

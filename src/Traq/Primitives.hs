{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives (
  primCallE,
  DefaultPrims (..),
  DefaultPrims',
  QSearchCFNW (..),
) where

import Control.Applicative ((<|>))
import GHC.Generics
import Text.Parsec (try)

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- | Build a primitive call by appropriately injecting an arbitrary primitive into a larger context.
primCallE :: forall ext ext'. (ext :<: ext') => ext -> P.Expr ext'
primCallE = P.PrimCallE . inject

data DefaultPrims sizeT precT
  = QAny (QSearchCFNW sizeT precT)
  | RAny (RandomSearch sizeT precT)
  | DAny (DetSearch sizeT precT)
  deriving (Eq, Show, Read, Generic)

type DefaultPrims' = DefaultPrims SizeT Double

type instance SizeType (DefaultPrims sizeT precT) = sizeT
type instance PrecType (DefaultPrims sizeT precT) = precT

instance P.MapSize (DefaultPrims size prec) where
  type MappedSize (DefaultPrims size prec) size' = DefaultPrims size' prec

  mapSize f (QAny p) = QAny (P.mapSize f p)
  mapSize f (RAny p) = RAny (P.mapSize f p)
  mapSize f (DAny p) = DAny (P.mapSize f p)

instance PrimAny sizeT precT :<: DefaultPrims sizeT precT where
  inject = QAny . inject

  project (QAny p) = project p
  project _ = Nothing

instance PrimSearch sizeT precT :<: DefaultPrims sizeT precT where
  inject = QAny . inject

  project (QAny p) = project p
  project _ = Nothing

-- Printing
instance PP.ToCodeString (DefaultPrims sizeT precT) where
  build (QAny (QAnyCFNW p)) = printSearchLikePrim "any" p
  build (QAny (QSearchCFNW p)) = printSearchLikePrim "search" p
  build (RAny p) = printSearchLikePrim "any_rand" p
  build (DAny p) = printSearchLikePrim "any_det" p

-- Parsing
instance P.Parseable (DefaultPrims sizeT precT) where
  parseE tp = try qany <|> try qsearch <|> try rany <|> try dany
   where
    qany = QAny . QAnyCFNW <$> parsePrimAnyWithName "any" tp
    qsearch = QAny . QSearchCFNW <$> parsePrimSearchWithName "search" tp
    rany = RAny . RandomSearch <$> parsePrimAnyWithName "any_rand" tp
    dany = DAny . DetSearch <$> parsePrimAnyWithName "any_det" tp

instance (P.TypingReqs sizeT) => P.TypeInferrable (DefaultPrims sizeT precT) sizeT
instance P.HasFreeVars (DefaultPrims sizeT precT)

-- Evaluation
instance (P.EvalReqs sizeT precT) => P.Evaluatable (DefaultPrims sizeT precT) sizeT precT

-- Costs
instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  P.UnitaryCost (DefaultPrims sizeT precT) sizeT precT

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  P.QuantumHavocCost (DefaultPrims sizeT precT) sizeT precT

instance
  ( Floating precT
  , Ord precT
  , Prob.ProbType precT
  , sizeT ~ SizeT
  , Show precT
  , P.EvalReqs sizeT precT
  ) =>
  P.QuantumExpCost (DefaultPrims sizeT precT) sizeT precT

-- Lowering
instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  ) =>
  CompileU.Lowerable (DefaultPrims sizeT precT) sizeT precT
  where
  lowerPrimitive delta (QAny q) = CompileU.lowerPrimitive delta q
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
  lowerPrimitive eps (QAny q) = CompileQ.lowerPrimitive eps q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

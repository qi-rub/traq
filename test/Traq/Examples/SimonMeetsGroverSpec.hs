{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SimonMeetsGroverSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import Traq.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives (QSearchCFNW (..))
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (assertRight, expectRight)

examplePath :: String
examplePath = "examples/simons_meets_grover.qb"

-- data SimonsSearch sizeT precT
--     = Simon (SimonsFindXorPeriod (Sym.Sym sizeT) precT)
--     | Search (QSearchCFNW sizeT precT)
--     deriving (Eq, Show, Read)

-- instance P.Parseable (SimonsSearch sizeT Double) where
--   parseE tp = (SimonsFindXorPeriod <$> P.parseE tp) <|> (QSearchCFNW <$> P.parseE tp)

spec :: Spec
spec = describe "Simon Meets Grover" $ do
  xit "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(QSearchCFNW (Sym.Sym SizeT) Double)) examplePath
    return ()

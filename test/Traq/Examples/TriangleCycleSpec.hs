{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

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
examplePath = "examples/triangle_finding.qb"

spec :: Spec
spec = describe "Triangle Cycle Finding" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(QSearchCFNW (Sym.Sym SizeT) Double)) examplePath
    return ()

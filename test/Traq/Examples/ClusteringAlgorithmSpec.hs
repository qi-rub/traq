{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ClusteringAlgorithmSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (QSearchCFNW (..))
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (expectRight)

examplePath :: String
examplePath = "examples/clustering_algorithm.qb"

spec :: Spec
spec = describe "Clustering Algorithm" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(QSearchCFNW (Sym.Sym SizeT) Double)) examplePath
    return ()

{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SteepMaxKSatSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.QMax
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (assertRight, expectRight)

examplePath :: String
examplePath = "examples/hillclimb/steep_max_sat.qb"

spec :: Spec
spec = describe "Steep max-k-sat" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(QMax (Sym.Sym SizeT) Double)) examplePath
    return ()

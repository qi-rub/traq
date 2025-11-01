{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (QSearchCFNW (..))
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type P = QSearchCFNW (Sym.Sym SizeT) Double

examplePath :: String
examplePath = "examples/triangle_finding.qb"

spec :: Spec
spec = describe "Triangle Cycle Finding" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

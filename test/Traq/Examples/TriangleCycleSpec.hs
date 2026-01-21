{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type P = DefaultPrims (Sym.Sym SizeT) Double

examplePath :: String
examplePath = "examples/triangle_finding.traq"

spec :: Spec
spec = describe "Triangle Cycle Finding" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

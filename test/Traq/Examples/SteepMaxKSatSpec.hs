{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SteepMaxKSatSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Class (Primitive)
import Traq.Primitives.Max.QMax
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (expectRight)

type P = Primitive (QMax (Sym.Sym SizeT) Double)

examplePath :: String
examplePath = "examples/hillclimb/steep_max_sat.qb"

spec :: Spec
spec = describe "Steep max-k-sat" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

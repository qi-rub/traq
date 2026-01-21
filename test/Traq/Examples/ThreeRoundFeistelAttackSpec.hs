{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ThreeRoundFeistelAttackSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Class (Primitive)
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (expectRight)

type P = Primitive (SimonsFindXorPeriod (Sym.Sym SizeT) Double)

examplePath :: String
examplePath = "examples/cryptanalysis/3_round_feistel.traq"

spec :: Spec
spec = describe "3 round feistel attack" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

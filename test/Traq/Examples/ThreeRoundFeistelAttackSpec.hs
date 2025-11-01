{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ThreeRoundFeistelAttackSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (expectRight)

examplePath :: String
examplePath = "examples/cryptanalysis/3_round_feistel.qb"

spec :: Spec
spec = describe "3 round feistel attack" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(SimonsFindXorPeriod (Sym.Sym SizeT) Double)) examplePath
    return ()

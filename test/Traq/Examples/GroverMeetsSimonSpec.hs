{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.GroverMeetsSimonSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/grover_meets_simon.qb"

type P = WorstCasePrims (Sym.Sym SizeT) Double

spec :: Spec
spec = describe "Grover Meets Simon" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

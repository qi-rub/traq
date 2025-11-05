{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.DepthThreeNandFormulaSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (expectRight)

examplePath :: String
examplePath = "examples/matrix_search/depth3_NAND_formula.qb"

spec :: Spec
spec = describe "Depth 3 NAND Formula" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.DepthThreeNandFormulaSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Compiler.Qualtran (toPy)
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/matrix_search/depth3_NAND_formula.traq"

loadExample :: IO (P.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con 8))
      & P.mapSize (Sym.subst "M" (Sym.con 4))
      & P.mapSize (Sym.subst "K" (Sym.con 2))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "Depth 3 NAND Formula" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

  describe "Compile" $ do
    let eps = A.failProb (0.1 :: Double)
    before (loadExample >>= expectRight . A.annotateProgWithErrorBudget eps) $ do
      it "lowers" $ \ex -> do
        assertRight $ Compiler.lowerProgram ex

      it "typechecks" $ \ex -> do
        ex_uqpl <- expectRight $ Compiler.lowerProgram ex
        assertRight $ CQPL.typeCheckProgram ex_uqpl

      it "cost" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
        let cost_from_analysis = getCost $ A.costQProg ex
        getCost cost `shouldBeLE` cost_from_analysis

      xit "target-py-qualtran" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        _ <- evaluate $ force $ toPy ex_cqpl
        return ()

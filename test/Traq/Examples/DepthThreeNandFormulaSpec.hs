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
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Qiskit as Qiskit
import qualified Traq.Compiler.Qualtran as Qualtran
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.QPL as QPL

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/matrix_search/depth3_NAND_formula.traq"

loadExample :: IO (CPL.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "N" (Sym.con 8))
      & CPL.mapSize (Sym.subst "M" (Sym.con 4))
      & CPL.mapSize (Sym.subst "K" (Sym.con 2))
      & CPL.mapSize Sym.unSym

spec :: Spec
spec = describe "Depth 3 NAND Formula" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

  describe "Compile" $ do
    let eps = A.failProb (0.1 :: Double)
    before (loadExample >>= expectRight . A.annotateProgWithErrorBudget eps) $ do
      it "lowers" $ \ex -> do
        assertRight $ Compiler.lowerProgram ex

      it "typechecks" $ \ex -> do
        ex_uqpl <- expectRight $ Compiler.lowerProgram ex
        assertRight $ QPL.typeCheckProgram ex_uqpl

      it "cost" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
        let cost_from_analysis = getCost $ A.costQProg ex
        getCost cost `shouldBeLE` cost_from_analysis

      xit "target-py-qualtran" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        _ <- evaluate $ force $ Qualtran.toPy ex_cqpl
        return ()

      xit "target-py-qiskit" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        _ <- evaluate $ force $ Qiskit.toPy ex_cqpl
        return ()

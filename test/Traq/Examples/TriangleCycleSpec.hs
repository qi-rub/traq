{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Experimental.Compiler.Qiskit as Qiskit
import qualified Traq.Experimental.Compiler.Qualtran as Qualtran
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

type P = DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double)

examplePath :: String
examplePath = "examples/search/triangle_finding.traq"

loadExample :: IO (CPL.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @P) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "N" (Sym.con 8))
      & CPL.mapSize Sym.unSym
      & CPL.mapPrec Sym.unSym

spec :: Spec
spec = describe "Triangle Cycle Finding" $ do
  describe "parses" $ do
    it "file" $ do
      expectRight =<< parseFromFile (CPL.programParser @P) examplePath
      return ()

    it "roundtrip" $ do
      prog <- expectRight =<< parseFromFile (CPL.programParser @P) examplePath
      p <- expectRight $ CPL.parseProgram @P $ PP.toCodeString prog
      p `shouldBe` prog

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)
    let load_prog = do
          ex <- loadExample
          expectRight $ A.annotateProgWithErrorBudget eps ex

    before load_prog $ do
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

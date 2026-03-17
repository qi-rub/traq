{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.GroverMeetsSimonSpec where

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
import Traq.Primitives
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/grover_meets_simon.traq"

type P = WorstCasePrims (Sym.Sym SizeT) Double

loadExample :: IO (CPL.Program (WorstCasePrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @P) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "n" (Sym.con 4))
      & CPL.mapSize Sym.unSym

spec :: Spec
spec = describe "Grover Meets Simon" $ do
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

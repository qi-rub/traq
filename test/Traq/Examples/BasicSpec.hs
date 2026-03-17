{-# LANGUAGE TypeApplications #-}

module Traq.Examples.BasicSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Qiskit as Qiskit
import qualified Traq.Compiler.Qualtran as Qualtran
import Traq.Prelude
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

type SymCore = Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "for loop" $ do
    describe "parses" $ do
      it "file" $ do
        p <- parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
        assertRight p

      it "roundtrip" $ do
        prog <- expectRight =<< parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
        p <- expectRight $ parseProgram @SymCore $ PP.toCodeString prog
        p `shouldBe` prog

    it "evaluates" $ do
      Right ex <- parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
      let ex' = mapSize (Sym.unSym . Sym.subst "N" 10 . Sym.subst "W" 20) ex
      let result = runProgram @Core' ex' mempty []

      result `shouldBeDistribution` [([FinV 10], 1.0)]

    describe "Compile" $ do
      let load_prog =
            parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
              >>= expectRight
                <&> mapSize (Sym.unSym . Sym.subst "N" 10 . Sym.subst "W" 20)
                <&> A.annotateProgWith (_exts A.annNoPrims)
              >>= expectRight

      beforeAll load_prog $ do
        it "lowers" $ \ex -> do
          assertRight $ Compiler.lowerProgram ex

        it "typechecks" $ \ex -> do
          ex_qpl <- expectRight $ Compiler.lowerProgram ex
          assertRight $ QPL.typeCheckProgram ex_qpl

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

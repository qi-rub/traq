{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGeneratorSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.CPL
import qualified Traq.Compiler as Compiler
import Traq.Examples.TreeGenerator
import qualified Traq.Experimental.Compiler.Qiskit as Qiskit
import qualified Traq.Experimental.Compiler.Qualtran as Qualtran
import Traq.Prelude
import Traq.Primitives
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

type Prim = DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double)
type Prim' = DefaultPrims SizeT Double

loadKnapsack ::
  -- | number of elements
  SizeT ->
  -- | bound on total weight
  SizeT ->
  -- | bound on total profit
  SizeT ->
  -- | number of iterations
  SizeT ->
  -- | bernoulli probability
  Double ->
  IO (Program Prim')
loadKnapsack n w p k prob = do
  Right prog <- parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.traq"
  return $
    prog
      & mapSize (Sym.subst "N" (Sym.con n))
      & mapSize (Sym.subst "W" (Sym.con w))
      & mapSize (Sym.subst "P" (Sym.con p))
      & mapSize (Sym.subst "K" (Sym.con k))
      & mapSize Sym.unSym
      & mapPrec (Sym.subst "p" (Sym.con prob))
      & mapPrec (Sym.subst "N" (Sym.con (fromIntegral n)))
      & mapPrec Sym.unSym

spec :: Spec
spec = do
  describe "Tree Generator Example" $ do
    describe "parses" $ do
      let prog = treeGeneratorExample @Prim (Sym.var "N") (Sym.var "W") (Sym.var "P") (Sym.var "K") (Sym.var "p")

      it "file" $ do
        p <- expectRight =<< parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.traq"
        p `shouldBe` prog

      it "roundtrip" $ do
        let prog' = mapPrec (Sym.subst "p" 0.2 . Sym.subst "N" 10) prog
        p <- expectRight $ parseProgram @Prim $ PP.toCodeString prog'
        p `shouldBe` prog'

    it "typechecks" $ do
      p <-
        parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.traq"
          >>= expectRight
      assertRight $ (typeCheckProg @Prim) p

    it "evaluates" $ do
      let n = 2
      prog <- loadKnapsack n 20 30 2 0.2
      let funInterpCtx =
            Map.fromList
              [ ("Capacity", const [FinV 20])
              , ("Profit", id)
              , ("Weight", id)
              ]
      let result = runProgram prog funInterpCtx []

      result
        `shouldBeDistribution` [ ([ArrV [FinV 0, FinV 1]], 0.8)
                               , ([ArrV [FinV 1, FinV 1]], 0.2)
                               ]

    describe "Compile" $ do
      let eps = A.failProb (0.0001 :: Double)
      let load_prog = do
            ex <- loadKnapsack 2 20 30 2 0.2
            expectRight $ A.annotateProgWith (_exts (A.annSinglePrim eps)) ex

      beforeAll load_prog $ do
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

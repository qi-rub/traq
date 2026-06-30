{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ClusteringAlgorithmSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/search/clustering.traq"

loadExample :: IO (CPL.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
  let prog' =
        prog
          & CPL.mapSize (Sym.subst "N" (Sym.con 8))
          & CPL.mapSize (Sym.subst "M" (Sym.con 4))
          & CPL.mapSize Sym.unSym
          & CPL.mapPrec Sym.unSym
  return prog'

spec :: Spec
spec = describe "Clustering Algorithm" $ do
  describe "parses" $ do
    it "file" $ do
      expectRight =<< parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
      return ()

    it "roundtrip" $ do
      prog <- expectRight =<< parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
      p <- expectRight $ CPL.parseProgram @(DefaultPrims (Sym.Sym SizeT) (Sym.Sym Double)) $ PP.toCodeString prog
      p `shouldBe` prog

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)
    let load_prog = do
          ex <- loadExample
          expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex

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

{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ClusteringAlgorithmSpec where

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
examplePath = "examples/clustering_algorithm.traq"

loadExample :: IO (CPL.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
  let prog' =
        prog
          & CPL.mapSize (Sym.subst "N" (Sym.con 8))
          & CPL.mapSize (Sym.subst "M" (Sym.con 4))
          & CPL.mapSize Sym.unSym
          & CPL.renameVars'
  return prog'

spec :: Spec
spec = describe "Clustering Algorithm" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ QPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ Qualtran.toPy ex_cqpl
      return ()

    xit "target-py-qiskit" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ Qiskit.toPy ex_cqpl
      return ()

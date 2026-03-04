{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ClusteringAlgorithmSpec where

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
examplePath = "examples/clustering_algorithm.traq"

loadExample :: IO (P.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
  let prog' =
        prog
          & P.mapSize (Sym.subst "N" (Sym.con 8))
          & P.mapSize (Sym.subst "M" (Sym.con 4))
          & P.mapSize Sym.unSym
          & P.renameVars'
  return prog'

spec :: Spec
spec = describe "Clustering Algorithm" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ P.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ CQPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- toPy ex_cqpl
      return ()

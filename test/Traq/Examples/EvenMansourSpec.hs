{-# LANGUAGE TypeApplications #-}

module Traq.Examples.EvenMansourSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Qiskit as Qiskit
import qualified Traq.Compiler.Qualtran as Qualtran
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Quantum
import qualified Traq.QPL as QPL

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/even_mansour.traq"

type SPrim size = Primitive (SimonsFindXorPeriod size Double)

loadEvenMansour ::
  -- | bitsize @n@ of the inputs/outputs
  SizeT ->
  IO (CPL.Program (SPrim SizeT))
loadEvenMansour n = do
  Right prog <- parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT))) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "N" (Sym.con (2 ^ n)))
      & CPL.mapSize (Sym.subst "n" (Sym.con n))
      & CPL.mapSize Sym.unSym

spec :: Spec
spec = describe "FindXorPeriod" $ do
  -- bitsize
  let n = 7 :: SizeT

  -- p0 matching the code (TODO remove the redundancy)
  let p0 = 0.01 :: Double

  it "parses" $ do
    expectRight =<< parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT))) examplePath
    return ()

  it "typechecks" $ do
    p <-
      parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT))) examplePath
        >>= expectRight
    assertRight $ CPL.typeCheckProg p

  before (loadEvenMansour n) $ do
    it "calculates unitary cost correctly" $ \program -> do
      let eps = A.failProb (0.01 :: Double)
      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costUProg prog'
      let formulaCost = 2 + 4 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

    it "calculates quantum max cost correctly" $ \program -> do
      let eps = A.failProb (0.01 :: Double)
      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costQProg prog'
      let formulaCost = 2 + 4 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

    describe "Compile" $ do
      let eps = A.failProb (0.0001 :: Double)

      it "lowers" $ \program -> do
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
        assertRight $ Compiler.lowerProgram ex'

      it "typechecks" $ \program -> do
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
        ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
        assertRight $ QPL.typeCheckProgram ex_uqpl

      it "cost" $ \program -> do
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
        let cost_from_analysis = getCost $ A.costQProg ex'
        getCost cost `shouldBeLE` cost_from_analysis

      xit "target-py-qualtran" $ \program -> do
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        _ <- evaluate $ force $ Qualtran.toPy ex_cqpl
        return ()

      xit "target-py-qiskit" $ \program -> do
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        _ <- evaluate $ force $ Qiskit.toPy ex_cqpl
        return ()

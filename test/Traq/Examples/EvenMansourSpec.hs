{-# LANGUAGE TypeApplications #-}

module Traq.Examples.EvenMansourSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Quantum
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/even_mansour.traq"

type SPrim size prec = Primitive (SimonsFindXorPeriod size prec)

loadEvenMansour ::
  -- | bitsize @n@ of the inputs/outputs
  SizeT ->
  IO (CPL.Program (SPrim SizeT Double))
loadEvenMansour n = do
  Right prog <- parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "N" (Sym.con (2 ^ n)))
      & CPL.mapSize (Sym.subst "n" (Sym.con n))
      & CPL.mapSize Sym.unSym
      & CPL.mapPrec Sym.unSym

spec :: Spec
spec = describe "FindXorPeriod" $ do
  -- bitsize
  let n = 7 :: SizeT

  -- p0 matching the code (TODO remove the redundancy)
  let p0 = 0.01 :: Double

  describe "parses" $ do
    it "file" $ do
      expectRight =<< parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
      return ()

    it "roundtrip" $ do
      prog <- expectRight =<< parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
      p <- expectRight $ CPL.parseProgram @(SPrim (Sym.Sym SizeT) (Sym.Sym Double)) $ PP.toCodeString prog
      p `shouldBe` prog

  it "typechecks" $ do
    p <-
      parseFromFile (CPL.programParser @(SPrim (Sym.Sym SizeT) (Sym.Sym Double))) examplePath
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
    let load_prog = do
          program <- loadEvenMansour n
          expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program

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

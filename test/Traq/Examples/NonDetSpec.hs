{-# LANGUAGE TypeApplications #-}

module Traq.Examples.NonDetSpec (spec) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Either (fromRight, isRight)
import qualified Data.Map as Map
import Text.Parsec.String (parseFromFile)

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CPL as CPL
import qualified Traq.CPL.Parser as CPLParser
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Compiler.Qualtran (toPy)
import Traq.Prelude
import Traq.Primitives (DefaultPrims)

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "SimpleExample" $ do
    let load = parseFromFile (CPLParser.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) "examples/primitives/nondet.traq"
    it "parses" $ do
      mEx <- load
      assertRight mEx

    let load' = load <&> fromRight (error "parsing failed") <&> CPL.mapSize Sym.unSym

    it "typechecks" $ do
      ex <- load'
      CPL.typeCheckProg ex `shouldSatisfy` isRight

    it "all solutions" $ do
      ex <- load'
      let oracleF = const [CPL.FinV 1]
      let out = CPL.runProgram ex (Map.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 0.1 :: Double)
                               | x <- [0 .. 9]
                               , let sigma = [CPL.FinV 1, CPL.FinV x]
                               ]

    it "no solutions" $ do
      ex <- load'
      let oracleF = const [CPL.FinV 0]
      let out = CPL.runProgram ex (Map.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 0.1 :: Double)
                               | x <- [0 .. 9]
                               , let sigma = [CPL.FinV 0, CPL.FinV x]
                               ]

    it "some solutions" $ do
      ex <- load'
      let sols = [1, 4, 6] :: [SizeT]
      let oracleF [CPL.FinV i] = [CPL.toValue $ i `elem` sols]
          oracleF _ = error "invalid input"
      let out = CPL.runProgram @_ @Double ex (Map.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 1 / 3 :: Double)
                               | x <- sols
                               , let sigma = [CPL.FinV 1, CPL.FinV x]
                               ]

    describe "Compile" $ do
      let eps = A.failProb (0.0001 :: Double)

      it "lowers" $ do
        ex <- load'
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
        assertRight $ Compiler.lowerProgram ex'

      it "typechecks" $ do
        ex <- load'
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
        ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
        assertRight $ CQPL.typeCheckProgram ex_uqpl

      it "cost" $ do
        ex <- load'
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
        let cost_from_analysis = getCost $ A.costQProg ex'
        getCost cost `shouldBeLE` cost_from_analysis

      xit "target-py-qualtran" $ do
        ex <- load'
        ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        _ <- evaluate $ force $ toPy ex_cqpl
        return ()

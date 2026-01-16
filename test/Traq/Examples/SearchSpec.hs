{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SearchSpec (spec) where

import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Examples.Search
import Traq.Prelude
import Traq.Primitives.Search.QSearchCFNW (_EQSearch, _QSearchZalka)
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "SearchSpec" $ do
  let ?epsilon = 1e-6 :: Double

  describe "arraySearch (no solutions)" $ do
    let n = 6
    let ex = arraySearch n

    it "type checks" $ do
      assertRight $ P.typeCheckProg ex

    let oracleF = const [P.FinV 0]
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram @_ @Double ex interpCtx []
      res `shouldBeDistribution` pure ([P.FinV 0], 1.0)

    let ecF = _EQSearch
    let ucF = _QSearchZalka

    it "unitary cost for eps=0.0001" $ do
      let eps = A.failProb (0.0001 :: Double)

      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      let computed_cost = getCost $ A.costUProg ex'

      let true_cost = 2 * ucF n eps
      computed_cost `shouldBe` true_cost

    it "quantum cost for eps=0.0001" $ do
      let eps = A.failProb (0.0001 :: Double)

      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      let computed_cost = getCost $ A.expCostQProg ex' [] interpCtx

      let true_cost = 2 * ecF n 0 eps
      computed_cost `shouldBe` true_cost

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "Unitary Compile" $ do
      let eps = A.failProb (0.0001 :: Double)

      it "lowers" $ do
        ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
        assertRight $ CompileU.lowerProgram ex'

      it "typechecks" $ do
        ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
        ex_uqpl <- expectRight $ CompileU.lowerProgram ex'
        assertRight $ CQPL.typeCheckProgram ex_uqpl

      it "preserves cost" $ do
        ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
        ex_uqpl <- expectRight $ CompileU.lowerProgram ex'
        let (uqpl_cost, _) = CQPL.programCost ex_uqpl
        let proto_cost = A.costUProg ex' :: SimpleQueryCost Double
        uqpl_cost `shouldSatisfy` (<= proto_cost)

  describe "arraySearch (returning solution)" $ do
    let n = 10
    let ex = arraySearchIx n

    it "type checks" $ do
      assertRight $ P.typeCheckProg ex

    let planted_sols = [2, 4, 5] :: [SizeT]
    let oracleF [P.FinV i] = [P.toValue $ i `elem` planted_sols]
        oracleF _ = error "invalid inputs"
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram @_ @Double ex interpCtx []

      res `shouldBeDistribution` [([P.FinV 1, P.FinV i], 1 / 3) | i <- planted_sols]

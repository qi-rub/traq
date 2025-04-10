module QCompose.Examples.MatrixSearchSpec (spec) where

import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Examples.MatrixSearch
import QCompose.Primitives.QSearch

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "matrix search example" $ do
    let (n, m) = (10, 10)
    let ex = matrixExampleS n m

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty ex

    it "has unique vars" $ do
      P.checkVarsUnique ex `shouldBe` True

    let oracleF = \[i, j] -> [if i == j then 1 else 0]
    it "evaluates" $ do
      let res = P.runProgram ex oracleF Ctx.empty
      res `shouldBe` pure (Ctx.singleton "result" 0)

    -- expected, worst, unitary
    let P.QSearchFormulas _ wcF ucF = cadeEtAlFormulas

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001 :: Double
      let cu = P.unitaryQueryCost cadeEtAlFormulas delta ex
      let nu_outer = ucF n (delta / 4)
      let nu_inner = 2 * ucF m (delta / 4 / nu_outer / 8)
      cu `shouldBe` 2 * nu_outer * 2 * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost cadeEtAlFormulas eps ex oracleF Ctx.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 2 / 4)
      cq `shouldBe` nq_outer * nq_inner

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    describe "lower to UQPL" $ do
      let delta = 0.001 :: Double
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram zalkaQSearch Ctx.empty delta ex

      it "type checks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram zalkaQSearch Ctx.empty delta ex
        assertRight $ UQPL.typeCheckProgram gamma ex_uqpl

      it "preserves cost" $ do
        (ex_uqpl, _) <- expectRight $ UQPL.lowerProgram zalkaQSearch Ctx.empty delta ex
        let (uqpl_cost, _) = UQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost cadeEtAlFormulas delta ex
        uqpl_cost `shouldBe` proto_cost

    describe "lower to CQPL" $ do
      let eps = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CQPL.lowerProgram qSearchCQImpl Ctx.empty eps ex

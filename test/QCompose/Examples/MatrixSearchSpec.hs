module QCompose.Examples.MatrixSearchSpec (spec) where

import Lens.Micro
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
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram ex interpCtx Ctx.empty
      res `shouldBe` pure (Ctx.singleton "result" 0)

    -- expected, worst, unitary
    let qformulas@(P.QSearchFormulas _ wcF ucF) = qsearchCFNW ^. to formulas
    let ualgo = qsearchCFNW ^. to unitaryAlgo
    let qalgo = qsearchCFNW ^. to quantumAlgo

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001 :: Double
      let cu = P.unitaryQueryCost qformulas delta ex "Oracle"
      let nu_outer = ucF n (delta / 4)
      let nu_inner = 2 * ucF m (delta / 4 / nu_outer / 8)
      cu `shouldBe` 2 * nu_outer * 2 * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost qformulas eps ex "Oracle" interpCtx Ctx.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 2 / 4)
      cq `shouldBe` nq_outer * nq_inner

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    describe "lower to UQPL" $ do
      let delta = 0.001 :: Double
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex

      it "type checks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        assertRight $ UQPL.typeCheckProgram gamma ex_uqpl

      it "preserves cost" $ do
        (ex_uqpl, _) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        let (uqpl_cost, _) = UQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost qformulas delta ex "Oracle"
        uqpl_cost `shouldBe` proto_cost

    describe "lower to CQPL" $ do
      let eps = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CQPL.lowerProgram qalgo Ctx.empty "Oracle" eps ex

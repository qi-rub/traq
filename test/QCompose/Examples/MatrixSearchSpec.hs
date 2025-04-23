module QCompose.Examples.MatrixSearchSpec (spec) where

import qualified Data.Number.Symbolic as Sym
import Lens.Micro

import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Examples.MatrixSearch
import QCompose.Primitives.QSearch
import QCompose.Primitives.Search.Prelude
import QCompose.Primitives.Search.Symbolic

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
    let (P.QSearchFormulas _ wcF ucF) = qsearchCFNW ^. to formulas

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001 :: Double
      let cu = P.unitaryQueryCost delta ex "Oracle"
      let nu_outer = ucF n (delta / 4)
      let nu_inner = 2 * ucF m (delta / 4 / nu_outer / 8)
      cu `shouldBe` 2 * nu_outer * 2 * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost eps ex "Oracle" interpCtx Ctx.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 16)
      let nq_oracle = 2
      cq `shouldBe` nq_outer * nq_inner * nq_oracle

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    describe "lower to UQPL" $ do
      let ualgo = qsearchCFNW ^. to unitaryAlgo
      let delta = 0.001 :: Double
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex

      it "type checks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        assertRight $ UQPL.typeCheckProgram gamma ex_uqpl

      it "preserves cost" $ do
        (ex_uqpl, _) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        let (uqpl_cost, _) = UQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost delta ex "Oracle"
        uqpl_cost `shouldBe` proto_cost

    describe "lower to CQPL" $ do
      let qalgo = qsearchCFNW ^. to quantumAlgo
      let eps = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CQPL.lowerProgram qalgo Ctx.empty "Oracle" eps ex

      it "type checks" $ do
        (ex_cqpl, gamma) <- expectRight $ CQPL.lowerProgram qalgo Ctx.empty "Oracle" eps ex
        -- case CQPL.typeCheckProgram gamma ex_uqpl of Left e -> putStrLn e; _ -> return ()
        assertRight $ CQPL.typeCheckProgram gamma ex_cqpl

  describe "matrix search symbolic" $ do
    let n = Sym.var "n" :: Sym.Sym Int
    let m = Sym.var "m" :: Sym.Sym Int
    let sbool = P.Fin (Sym.con 2) :: P.VarType (Sym.Sym Int)
    let ex = matrixExample n m sbool

    -- expected, worst, unitary
    let (P.QSearchFormulas _ wcF ucF) = qsearchSymbolic ^. to formulas

    it "unitary cost for delta=0.0001" $ do
      let delta = Sym.var "δ" :: Sym.Sym Double
      let cu = P.unitaryQueryCost delta ex "Oracle"
      let nu_outer = ucF n (delta / 2 / 2)
      let nu_inner = 2 * ucF m ((delta / 2 - delta / 2 / 2) / nu_outer / 2 / 2 / 2)
      let nu_oracle = 2
      cu `shouldBe` 2 * nu_outer * nu_inner * nu_oracle

    it "quantum cost for eps=0.0001" $ do
      let eps = Sym.var "ε" :: Sym.Sym Double
      let cq = P.quantumMaxQueryCost eps ex "Oracle"
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m ((eps - eps / 2) / nq_outer / 2 / 2 / 2 / 2)
      let nq_oracle = 2
      cq `shouldBe` nq_outer * nq_inner * nq_oracle

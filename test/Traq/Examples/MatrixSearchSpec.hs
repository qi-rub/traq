{-# LANGUAGE TypeApplications #-}

module Traq.Examples.MatrixSearchSpec (spec) where

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Examples.MatrixSearch
import Traq.Primitives (Primitive (..))
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW (_EQSearchWorst, _QSearchZalkaWithNormErr)
import Traq.Primitives.Search.Symbolic
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "matrix search example" $ do
    let (n, m) = (5, 5)
    let ex = matrixExampleS n m

    it "type checks" $ do
      assertRight $ P.typeCheckProg ex

    it "has unique vars" $ do
      P.checkVarsUnique ex `shouldBe` True

    let oracleF = \[P.FinV i, P.FinV j] -> [P.toValue $ i == j]
    let interpCtx = Ctx.singleton "Matrix" oracleF

    it "evaluates" $ do
      let res = P.runProgram @_ @Double ex interpCtx []
      res `shouldBeDistribution` pure ([P.FinV 0], 1.0)

    -- expected, worst, unitary
    let wcF = _EQSearchWorst
    let ucF = _QSearchZalkaWithNormErr

    it "unitary cost for delta=0.0001" $ do
      let delta = P.l2NormError (0.001 :: Double)
      let cu = getCost $ P.unitaryQueryCost P.SplitSimple delta ex
      let nu_outer = ucF n (delta `P.divideError` 2)
      let nu_inner = 2 * ucF m (delta `P.divideError` (2 * nu_outer * 4))
      cu `shouldBe` 2 * nu_outer * 2 * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = P.failProb (0.001 :: Double)
      let cq = getCost $ P.quantumQueryCost P.SplitSimple eps ex interpCtx []
      let nq_outer = wcF n (eps `P.divideError` 2)
      let nq_inner = 2 * ucF m (P.requiredFailProbToNormError eps `P.divideError` (2 * nq_outer * 4))
      let nq_oracle = 2
      cq `shouldBe` 2 * nq_outer * nq_inner * nq_oracle

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "Unitary Compile" $ do
      let delta = P.l2NormError (0.001 :: Double)
      it "lowers" $ do
        assertRight $ CompileU.lowerProgram default_ Ctx.empty delta ex

      it "type checks" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty delta ex
        let tc_res = CQPL.typeCheckProgram ex_uqpl
        either print (const $ pure ()) tc_res
        assertRight tc_res

      it "preserves cost" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty delta ex
        let uqpl_cost = getCost . fst $ CQPL.programCost ex_uqpl
        let proto_cost = getCost $ P.unitaryQueryCost P.SplitSimple delta ex
        uqpl_cost `shouldSatisfy` (<= proto_cost)

    describe "lower to CQPL" $ do
      let eps = P.failProb (0.001 :: Double)
      it "lowers" $ do
        assertRight $ CompileQ.lowerProgram default_ Ctx.empty eps ex

      it "type checks" $ do
        ex_cqpl <- expectRight $ CompileQ.lowerProgram default_ Ctx.empty eps ex
        -- case CQPL.typeCheckProgram gamma ex_uqpl of Left e -> putStrLn e; _ -> return ()
        assertRight $ CQPL.typeCheckProgram ex_cqpl

  xdescribe "matrix search symbolic" $ do
    let n = Sym.var "n" :: Sym.Sym Int
    let m = Sym.var "m" :: Sym.Sym Int
    let ex = mkMatrixExample (\ty f -> P.PrimCallE $ Primitive [f] $ QSearchSym $ PrimSearch AnyK ty) n m

    -- expected, worst, unitary
    let ucF = _QryU
    let wcF = _QryQmax

    -- TODO: update the tests below once symbolic expressions are upgraded.

    it "unitary cost" $ do
      let delta = P.l2NormError (Sym.var "δ" :: Sym.Sym Double)
      let cu = getCost $ P.unitaryQueryCost P.SplitSimple delta ex

      let delta_outer = delta `P.divideError` 2
      let nu_outer = ucF n delta_outer
      let nu_inner = 2 * ucF m ((delta - delta_outer) `P.divideError` (nu_outer * 2 * 2 * 2))
      let nu_oracle = 2
      let from_formula = 2 * nu_outer * nu_inner * nu_oracle
      -- cu `shouldBe` from_formula
      show cu `shouldBe` "((QryU(n, δ/2.0)) .* (((2.0) .* (0.0+((QryU(m, (δ-δ/2.0)/QryU(n, δ/2.0)/2.0/2.0/2.0)) .* (((2.0) .* (0.0+((2.0) .* (1.0))+0.0))))+0.0))))"

    it "unitary cost (optimized precision splitting)" $ do
      let delta = P.l2NormError (Sym.var "δ" :: Sym.Sym Double)
      let cu = getCost $ P.unitaryQueryCost P.SplitUsingNeedsEps delta ex

      let delta_outer = delta `P.divideError` 2
      let nu_outer = ucF n delta_outer
      let nu_inner = 2 * ucF m ((delta - delta_outer) `P.divideError` (nu_outer * 2))
      let nu_oracle = 2
      let from_formula = 2 * nu_outer * nu_inner * nu_oracle
      -- cu `shouldBe` from_formula
      show cu `shouldBe` "((QryU(n, δ/2.0)) .* (((2.0) .* (0.0+((QryU(m, (δ-δ/2.0)/QryU(n, δ/2.0)/2.0)) .* (((2.0) .* (0.0+((2.0) .* (1.0))+0.0))))+0.0))))"

    it "quantum worst case cost" $ do
      let eps = P.failProb (Sym.var "ε" :: Sym.Sym Double)
      let cq = getCost $ P.quantumMaxQueryCost P.SplitSimple eps ex

      let eps_outer = eps `P.divideError` 2
      let nq_outer = wcF n eps_outer
      let nq_inner = 2 * ucF m (P.requiredFailProbToNormError (eps - eps_outer) `P.divideError` (nq_outer * 2 * 2 * 2))
      let nq_oracle = 2
      let from_formula = 2 * nq_outer * nq_inner * nq_oracle
      -- cq `shouldBe` from_formula
      show cq `shouldBe` "((QryQmax(n, ε/2.0)) .* (((2.0) .* (0.0+((QryU(m, (ε-ε/2.0)/QryQmax(n, ε/2.0)/2.0/2.0/2.0/2.0)) .* (((2.0) .* (0.0+((2.0) .* (1.0))+0.0))))+0.0))))"

    it "quantum worst case cost (optimized precision splitting)" $ do
      let eps = P.failProb (Sym.var "ε" :: Sym.Sym Double)
      let cq = getCost $ P.quantumMaxQueryCost P.SplitUsingNeedsEps eps ex

      let eps_outer = eps `P.divideError` 2
      let nq_outer = wcF n eps_outer
      let nq_inner = 2 * ucF m (P.requiredFailProbToNormError (eps - eps_outer) `P.divideError` (nq_outer * 2))
      let nq_oracle = 2
      let from_formula = 2 * nq_outer * nq_inner * nq_oracle
      -- cq `shouldBe` from_formula
      show cq `shouldBe` "((QryQmax(n, ε/2.0)) .* (((2.0) .* (0.0+((QryU(m, (ε-ε/2.0)/QryQmax(n, ε/2.0)/2.0/2.0)) .* (((2.0) .* (0.0+((2.0) .* (1.0))+0.0))))+0.0))))"

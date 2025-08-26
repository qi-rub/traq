{-# LANGUAGE TypeApplications #-}

module Traq.Examples.MatrixSearchSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Examples.MatrixSearch
import Traq.Primitives.Search.QSearchCFNW (_EQSearchWorst, _QSearchZalka)
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
    let uticks = mempty & at "Matrix" ?~ 1.0
    let cticks = mempty & at "Matrix" ?~ 1.0

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
    let ucF = _QSearchZalka

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001 :: Double
      let cu = P.unitaryQueryCost P.SplitSimple delta ex uticks
      let nu_outer = ucF n (delta / 2)
      let nu_inner = 2 * ucF m (delta / 2 / nu_outer / 8)
      cu `shouldBe` 2 * nu_outer * 2 * nu_inner

    xit "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost P.SplitSimple eps ex cticks uticks interpCtx []
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 16)
      let nq_oracle = 2
      cq `shouldBe` 2 * nq_outer * nq_inner * nq_oracle

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "Unitary Compile" $ do
      let delta = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CompileU.lowerProgram default_ Ctx.empty uticks delta ex

      it "type checks" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty uticks delta ex
        let tc_res = CQPL.typeCheckProgram ex_uqpl
        either print (const $ pure ()) tc_res
        assertRight tc_res

      it "preserves cost" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty uticks delta ex
        let (uqpl_cost, _) = CQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost P.SplitSimple delta ex uticks
        uqpl_cost `shouldSatisfy` (<= proto_cost)

    describe "lower to CQPL" $ do
      let eps = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CompileQ.lowerProgram default_ Ctx.empty uticks cticks eps ex

      it "type checks" $ do
        ex_cqpl <- expectRight $ CompileQ.lowerProgram default_ Ctx.empty uticks cticks eps ex
        -- case CQPL.typeCheckProgram gamma ex_uqpl of Left e -> putStrLn e; _ -> return ()
        assertRight $ CQPL.typeCheckProgram ex_cqpl

  describe "matrix search symbolic" $ do
    let n = Sym.var "n" :: Sym.Sym Int
    let m = Sym.var "m" :: Sym.Sym Int
    let ex = matrixExample @QSearchSym n m
    let uticks = mempty & at "Matrix" ?~ 1.0
    let cticks = mempty & at "Matrix" ?~ 1.0

    -- expected, worst, unitary
    let ucF = _QryU
    let wcF = _QryQmax

    it "unitary cost" $ do
      let delta = Sym.var "δ" :: Sym.Sym Double
      let cu = P.unitaryQueryCost P.SplitSimple delta ex uticks
      let nu_outer = ucF n (delta / 2)
      let nu_inner = 2 * ucF m ((delta - delta / 2) / nu_outer / 2 / 2 / 2)
      let nu_oracle = 2
      cu `shouldBe` 2 * nu_outer * nu_inner * nu_oracle

    it "unitary cost (optimized precision splitting)" $ do
      let delta = Sym.var "δ" :: Sym.Sym Double
      let cu = P.unitaryQueryCost P.SplitUsingNeedsEps delta ex uticks
      let nu_outer = ucF n (delta / 2)
      let nu_inner = 2 * ucF m ((delta - delta / 2) / nu_outer / 2)
      let nu_oracle = 2
      cu `shouldBe` 2 * nu_outer * nu_inner * nu_oracle

    it "quantum worst case cost" $ do
      let eps = Sym.var "ε" :: Sym.Sym Double
      let cq = P.quantumMaxQueryCost P.SplitSimple eps ex uticks cticks
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m ((eps - eps / 2) / nq_outer / 2 / 2 / 2 / 2)
      let nq_oracle = 2
      cq `shouldBe` 2 * nq_outer * nq_inner * nq_oracle

    it "quantum worst case cost (optimized precision splitting)" $ do
      let eps = Sym.var "ε" :: Sym.Sym Double
      let cq = P.quantumMaxQueryCost P.SplitUsingNeedsEps eps ex uticks cticks
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m ((eps - eps / 2) / nq_outer / 2 / 2)
      let nq_oracle = 2
      cq `shouldBe` 2 * nq_outer * nq_inner * nq_oracle

{-# LANGUAGE TypeApplications #-}

module Traq.Examples.MatrixSearchSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Quantum as CQPL
import qualified Traq.Compiler.Unitary as UQPL
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import qualified Traq.Utils.Printing as PP

import Traq.Examples.MatrixSearch
import Traq.Primitives.Search.QSearchCFNW (_EQSearchWorst, _QSearchZalka)
import Traq.Primitives.Search.Symbolic

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "matrix search example" $ do
    let (n, m) = (5, 5)
    let ex = matrixExampleS n m
    let uticks = mempty & at "Oracle" ?~ 1.0
    let cticks = mempty & at "Oracle" ?~ 1.0

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
    let wcF = _EQSearchWorst
    let ucF = _QSearchZalka

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001 :: Double
      let cu = P.unitaryQueryCost P.SplitSimple delta ex uticks
      let nu_outer = ucF n (delta / 4)
      let nu_inner = 2 * ucF m (delta / 4 / nu_outer / 8)
      cu `shouldBe` 2 * 2 * nu_outer * 2 * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost P.SplitSimple eps ex cticks uticks interpCtx Ctx.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 16)
      let nq_oracle = 2
      cq `shouldBe` 2 * nq_outer * nq_inner * nq_oracle

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "lower to UQPL" $ do
      let delta = 0.001 :: Double
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram default_ Ctx.empty uticks delta ex

      it "type checks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram default_ Ctx.empty uticks delta ex
        let tc_res = UQPL.typeCheckProgram gamma ex_uqpl
        either print (const $ pure ()) tc_res
        assertRight tc_res

      it "preserves cost" $ do
        (ex_uqpl, _) <- expectRight $ UQPL.lowerProgram default_ Ctx.empty uticks delta ex
        let (uqpl_cost, _) = UQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost P.SplitSimple delta ex uticks
        uqpl_cost `shouldSatisfy` (<= proto_cost)

    describe "lower to CQPL" $ do
      let eps = 0.001 :: Double
      it "lowers" $ do
        assertRight $ CQPL.lowerProgram default_ Ctx.empty uticks cticks eps ex

      it "type checks" $ do
        ex_cqpl <- expectRight $ CQPL.lowerProgram default_ Ctx.empty uticks cticks eps ex
        -- case CQPL.typeCheckProgram gamma ex_uqpl of Left e -> putStrLn e; _ -> return ()
        assertRight $ CQPL.typeCheckProgram ex_cqpl

  describe "matrix search symbolic" $ do
    let n = Sym.var "n" :: Sym.Sym Int
    let m = Sym.var "m" :: Sym.Sym Int
    let sbool = P.Fin (Sym.con 2) :: P.VarType (Sym.Sym Int)
    let ex = matrixExample @QSearchSym n m sbool
    let uticks = mempty & at "Oracle" ?~ 1.0
    let cticks = mempty & at "Oracle" ?~ 1.0

    -- expected, worst, unitary
    let ucF = _QryU
    let wcF = _QryQmax

    it "unitary cost" $ do
      let delta = Sym.var "δ" :: Sym.Sym Double
      let cu = P.unitaryQueryCost P.SplitSimple delta ex uticks
      let nu_outer = ucF n (delta / 2 / 2)
      let nu_inner = 2 * ucF m ((delta / 2 - delta / 2 / 2) / nu_outer / 2 / 2 / 2)
      let nu_oracle = 2
      cu `shouldBe` 4 * nu_outer * nu_inner * nu_oracle

    it "unitary cost (optimized precision splitting)" $ do
      let delta = Sym.var "δ" :: Sym.Sym Double
      let cu = P.unitaryQueryCost P.SplitUsingNeedsEps delta ex uticks
      let nu_outer = ucF n (delta / 2 / 2)
      let nu_inner = 2 * ucF m ((delta / 2 - delta / 2 / 2) / nu_outer / 2)
      let nu_oracle = 2
      cu `shouldBe` 4 * nu_outer * nu_inner * nu_oracle

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

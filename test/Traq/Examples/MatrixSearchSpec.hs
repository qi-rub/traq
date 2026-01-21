{-# LANGUAGE TypeApplications #-}

module Traq.Examples.MatrixSearchSpec (spec) where

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Examples.MatrixSearch
import Traq.Primitives (Primitive (..))
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW (_EQSearchWorst, _QSearchZalka)
import Traq.Primitives.Search.Symbolic
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "MatrixSearch" $ do
  describe "5x5 example" $ do
    let (n, m) = (5, 5)
    let ex = matrixExampleS n m

    it "type checks" $ do
      assertRight $ P.typeCheckProg ex

    it "has unique vars" $ do
      P.checkVarsUnique ex `shouldBe` True

    let oracleF = \case
          [P.FinV i, P.FinV j] -> [P.toValue $ i == j]
          _ -> undefined
    let interpCtx = Ctx.singleton "Matrix" oracleF

    it "evaluates" $ do
      let res = P.runProgram @_ @Double ex interpCtx []
      res `shouldBeDistribution` pure ([P.FinV 0], 1.0)

    -- worst, unitary
    let wcF = _EQSearchWorst
    let ucF = _QSearchZalka

    it "unitary cost for delta=0.0001" $ do
      let eps = A.failProb (0.001 :: Double)

      ex' <- expectRight $ A.annotateProgWithErrorBudgetU eps ex
      let cu = getCost $ A.costUProg ex'

      let eps_outer = A.splitFailProb eps 2
      let eps_inner = eps - eps_outer
      let nu_outer = 2 * ucF n eps_outer
      let nu_inner = 2 * ucF m (A.unitarySubroutineTVBudget (eps_inner `A.splitFailProb` nu_outer))
      cu `shouldBe` nu_outer * nu_inner

    it "quantum cost for eps=0.001" $ do
      let eps = A.failProb (0.001 :: Double)

      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      let cq = getCost $ A.expCostQProg ex' [] interpCtx

      let eps_outer = eps `A.splitFailProb` 2
      let eps_inner = eps - eps_outer
      let nq_outer = 2 * wcF n eps_outer
      let nq_inner = 2 * ucF m (A.unitarySubroutineTVBudget (eps_inner `A.splitFailProb` nq_outer))
      cq `shouldBe` nq_outer * nq_inner

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "Unitary Compile" $ do
      let eps = A.failProb (0.001 :: Double)
      it "lowers" $ do
        pendingWith "TODO: unitary compile prims"
        ex' <- expectRight $ A.annotateProgWithErrorBudgetU eps ex
        assertRight $ Compiler.lowerProgramU ex'

      it "type checks" $ do
        pendingWith "TODO: unitary compile prims"
        ex' <- expectRight $ A.annotateProgWithErrorBudgetU eps ex
        ex_uqpl <- expectRight $ Compiler.lowerProgramU ex'
        let tc_res = CQPL.typeCheckProgram ex_uqpl
        either print (const $ pure ()) tc_res
        assertRight tc_res

      it "preserves cost" $ do
        pendingWith "TODO: unitary compile prims"
        ex' <- expectRight $ A.annotateProgWithErrorBudgetU eps ex
        ex_uqpl <- expectRight $ Compiler.lowerProgramU ex'
        let uqpl_cost = getCost . fst $ CQPL.programCost ex_uqpl
        let proto_cost = getCost $ A.costUProg ex'
        uqpl_cost `shouldSatisfy` (<= proto_cost)

    describe "lower to CQPL" $ do
      let eps = A.failProb (0.001 :: Double)
      it "lowers" $ do
        ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
        assertRight $ Compiler.lowerProgram ex'

      fit "type checks" $ do
        ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
        putStrLn $ PP.toCodeString ex_cqpl
        assertRight $ CQPL.typeCheckProgram ex_cqpl

  describe "symbolic" $ do
    let n = Sym.var "n" :: Sym.Sym Int
    let m = Sym.var "m" :: Sym.Sym Int

    let ex_no_ann = mkMatrixExample (\ty f -> P.PrimCallE $ Primitive [f] $ QSearchSym @Int @Double $ PrimSearch AnyK ty) n m
    let get_ann_ex = either fail pure $ A.annSymEpsProg ex_no_ann

    let eps_inner = A.failProb (Sym.var "eps_0" :: Sym.Sym Double)
    let eps_outer = A.failProb (Sym.var "eps_1" :: Sym.Sym Double)

    let ucF = _QryU -- unitary
    let wcF = _QryQmax -- quantum

    -- test the annotated program
    before get_ann_ex $ do
      it "unitary cost" $ \ex -> do
        let cu = Sym.simpl $ getCost $ A.costUProg ex

        let nu_inner = 2 * ucF m eps_inner
        let nu_outer = 2 * ucF n eps_outer
        let from_formula = nu_outer * nu_inner

        cu `shouldBe` from_formula

      it "unitary error" $ \ex -> do
        let err_u = A.traceNormErrorUProg ex

        let eps_sub = A.unitarySubroutineTVError eps_inner
        let eps_sub_tot = A.failProb (2.0 * ucF n eps_outer) * eps_sub
        let from_formula = eps_outer + eps_sub_tot

        err_u `shouldBe` from_formula

      it "quantum worst case cost" $ \ex -> do
        let cq = Sym.simpl $ getCost $ A.costQProg ex

        let nu_inner = 2 * ucF m eps_inner
        let nq_outer = 2 * wcF n eps_outer
        let from_formula = nq_outer * nu_inner

        cq `shouldBe` from_formula

      it "quantum error" $ \ex -> do
        let err_q = A.tvErrorQProg ex

        let eps_sub = A.unitarySubroutineTVError eps_inner
        let eps_sub_tot = A.failProb (2.0 * wcF n eps_outer) * eps_sub
        let from_formula = eps_outer + eps_sub_tot

        err_q `shouldBe` from_formula

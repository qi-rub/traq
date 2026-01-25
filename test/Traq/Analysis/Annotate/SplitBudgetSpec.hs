module Traq.Analysis.Annotate.SplitBudgetSpec (spec) where

import qualified Data.Map as Map

import Lens.Micro.GHC

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.Examples.MatrixSearch (matrixExampleS)
import Traq.Primitives.Search.QSearchCFNW
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "annotate-budget" $ do
  describe "matrix example" $ do
    let n = 10
    let m = 10
    let eps_tot = A.failProb (0.001 :: Double)

    let prog = matrixExampleS n m

    it "annotates" $ do
      assertRight $ A.annotateProgWithErrorBudget eps_tot prog

    it "computes error" $ do
      prog' <- expectRight $ A.annotateProgWithErrorBudget eps_tot prog
      let eps_ann = A.tvErrorQProg prog'
      eps_ann `shouldSatisfy` (<= eps_tot)
    it "computes cost" $ do
      prog' <- expectRight $ A.annotateProgWithErrorBudget eps_tot prog
      let fn_interps = Map.singleton "Matrix" (\case [P.FinV i, P.FinV j] -> [P.toValue (i == j)]; _ -> undefined)
      let cost = getCost $ A.expCostQProg prog' [] fn_interps

      let eps_outer = eps_tot `A.splitFailProb` 2 :: A.FailProb Double
      let q_outer = _EQSearchWorst n eps_outer * 2

      let eps_inner =
            (eps_tot - eps_outer)
              & (`A.splitFailProb` q_outer)
              & A.unitarySubroutineTVBudget
      let q_inner = _QSearchZalka m eps_inner * 2

      let formula_cost = q_outer * q_inner

      cost `shouldBe` formula_cost

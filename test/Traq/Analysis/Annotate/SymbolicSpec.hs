{-# LANGUAGE TypeApplications #-}

module Traq.Analysis.Annotate.SymbolicSpec (spec) where

import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import Traq.Analysis.Annotate.Symbolic
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.Examples.MatrixSearch (matrixExampleS)
import Traq.Primitives.Class ()
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "annotate-symbolic" $ do
  describe "matrix example" $ do
    let prog = matrixExampleS 10 10
    it "annotates" $ do
      assertRight $ annSymEpsProg @Double prog
    it "computes error" $ do
      prog' <- expectRight $ annSymEpsProg @Double prog
      let eps_tot = A.tvErrorQProg prog'
      show eps_tot `shouldBe` "FailProb (eps_1+58.18590894709818*log (1.0/eps_1)*sqrt (2.0*eps_0))"
    it "computes cost" $ do
      prog' <- expectRight $ annSymEpsProg @Double prog
      let fn_interps = Ctx.singleton "Matrix" (\case [P.FinV i, P.FinV j] -> [P.toValue (i == j)]; _ -> undefined)
      let cost = getCost $ A.expCostQProg prog' [] fn_interps
      show cost `shouldBe` "0.0+(0.0+((58.18590894709818*log (1.0/eps_1)) .* (0.0+(0.0+((6.0*logBase 0.6086 eps_0) .* (1.0)))+0.0)))+0.0"

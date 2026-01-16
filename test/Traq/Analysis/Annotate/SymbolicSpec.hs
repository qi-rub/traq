{-# LANGUAGE TypeApplications #-}

module Traq.Analysis.Annotate.SymbolicSpec (spec) where

import Control.Monad (forM_)

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.Annotate.Symbolic
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.Examples.MatrixSearch (matrixExampleS)
import Traq.Primitives.Class ()
import Traq.Primitives.Search.QSearchCFNW (_EQSearch, _EQSearchWorst, _QSearchZalka)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

data MatrixInput = MatrixInput {fn :: Int -> Int -> Bool, n, t :: Int}

spec :: Spec
spec = describe "annotate-symbolic" $ do
  let eps_1 = A.failProb (Sym.var "eps_1" :: Sym.Sym Double)
  let eps_0 = A.failProb (Sym.var "eps_0" :: Sym.Sym Double)

  describe "matrix example" $ do
    let m = 10
    let prog_10 = matrixExampleS m m
    it "annotates" $ do
      assertRight $ annSymEpsProg @Double prog_10
    it "computes error" $ do
      prog_10' <- expectRight $ annSymEpsProg @Double prog_10
      let eps_tot = A.tvErrorQProg prog_10'

      let nq_outer = 2 * _EQSearchWorst m eps_1
      let eps_formula = eps_1 + A.unitarySubroutineTVErrorTotal nq_outer eps_0

      eps_tot `shouldBe` eps_formula

    describe "computes exp cost" $ do
      let mats =
            [ MatrixInput{fn = \i j -> i == j, n = 10, t = 0}
            , MatrixInput{fn = \i _ -> i < 3, n = 10, t = 3}
            , MatrixInput{fn = \_ _ -> True, n = 10, t = 10}
            ]

      forM_ (zip [1 :: Int ..] mats) $ \(ix, MatrixInput{fn, n, t}) -> do
        it ("case#" ++ show ix) $ do
          let prog = matrixExampleS n n
          prog' <- expectRight $ annSymEpsProg @Double prog

          let fn_interps = Ctx.singleton "Matrix" (\case [P.FinV i, P.FinV j] -> [P.toValue $ fn i j]; _ -> undefined)
          let cost = Sym.simpl $ getCost $ A.expCostQProg prog' [] fn_interps

          let nq_outer = 2 * _EQSearch n t eps_1
          let nu_inner = 2 * _QSearchZalka n eps_0

          cost `shouldBe` nq_outer * nu_inner

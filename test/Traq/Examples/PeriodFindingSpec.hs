{-# LANGUAGE TypeApplications #-}

module Traq.Examples.PeriodFindingSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type SPrim size = Primitive (SimonsFindXorPeriod size Double)

examplePath :: String
examplePath = "examples/cryptanalysis/period_finding.qb"

loadPeriodFinding ::
  -- | bitsize @n@ of the inputs/outputs
  SizeT ->
  IO (P.Program (SPrim SizeT))
loadPeriodFinding n = do
  Right prog <- parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con $ 2 ^ n))
      & P.mapSize (Sym.subst "n" (Sym.con n))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "FindXorPeriod" $ do
  -- bitsize
  let n = 7 :: SizeT

  -- p0 matching the code (TODO remove the redundancy)
  let p0 = 0.01 :: Double

  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
    return ()

  it "typechecks" $ do
    p <-
      parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
        >>= expectRight
    assertRight $ (P.typeCheckProg @(SPrim (Sym.Sym SizeT))) p

  before (loadPeriodFinding n) $ do
    it "calculates unitary cost correctly" $ \program -> do
      let eps = A.failProb (0.1 :: Double)
      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costUProg prog'
      let formulaCost = 2 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

    it "calculates quantum max cost correctly" $ \program -> do
      let eps = A.failProb (0.1 :: Double)
      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costQProg prog'
      let formulaCost = 2 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

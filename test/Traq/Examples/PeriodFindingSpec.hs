{-# LANGUAGE TypeApplications #-}

module Traq.Examples.PeriodFindingSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import Traq.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (assertRight, expectRight)

examplePath :: String
examplePath = "examples/cryptoanalysis/period_finding.qb"

loadPeriodFinding ::
  -- | bitsize @n@ of the inputs/outputs
  SizeT ->
  IO (P.Program (SimonsFindXorPeriod SizeT Double))
loadPeriodFinding n = do
  Right prog <- parseFromFile (P.programParser @(SimonsFindXorPeriod (Sym.Sym SizeT) Double)) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con $ 2 ^ n))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "FindXorPeriod" $ do
  -- bitsize
  let n = 7 :: SizeT

  -- p0 matching the code (TODO remove the redundancy)
  let p0 = 0.01 :: Double

  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(SimonsFindXorPeriod (Sym.Sym SizeT) Double)) examplePath
    return ()

  it "typechecks" $ do
    p <-
      parseFromFile (P.programParser @(SimonsFindXorPeriod (Sym.Sym SizeT) Double)) examplePath
        >>= expectRight
    assertRight $ (P.typeCheckProg @(SimonsFindXorPeriod (Sym.Sym SizeT) Double)) p

  before (loadPeriodFinding n) $ do
    it "calculates unitary cost correctly" $ \program -> do
      let delta = P.l2NormError (0.01 :: Double)
      let actualCost = getCost $ P.unitaryQueryCost P.SplitUsingNeedsEps delta program
      let formulaCost = 2 * _SimonsQueries (fromIntegral n) p0 (P.requiredNormErrorToFailProb $ delta `P.divideError` 2)

      actualCost `shouldBe` formulaCost

    it "calculates quantum max cost correctly" $ \program -> do
      let eps = P.failProb (0.1 :: Double)
      let actualCost = getCost $ P.quantumMaxQueryCost P.SplitUsingNeedsEps eps program
      let formulaCost = 2 * _SimonsQueries (fromIntegral n) p0 (eps `P.divideError` 2)

      actualCost `shouldBe` formulaCost

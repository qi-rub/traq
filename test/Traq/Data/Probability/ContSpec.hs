{-# LANGUAGE TypeApplications #-}

module Traq.Data.Probability.ContSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Probability as Prob

import Test.Hspec
import TestHelpers

ex1 :: Prob.ExpMonad Double Int
ex1 = do
  x <- Prob.uniform [0, 0, 1, 2]
  if x == 0 then Prob.uniform [1 .. 6] else pure x

spec :: Spec
spec = describe "ExpMonad" $ do
  describe "Expectation" $ do
    it "mass" $ do
      let mu = Prob.bernoulli 0.3 :: Prob.ExpMonad Double Bool
      Prob.mass mu `shouldBe` 1.0
      Prob.mass (Prob.choose [(0.5, mu)]) `shouldBe` 0.5
      Prob.mass (Prob.choose2 0.5 mu Prob.zero) `shouldBe` 0.5
      Prob.mass (Prob.choose2 0.5 mu mu) `shouldBe` 1
      Prob.mass ex1 `shouldBe` 1
    it "expectation" $ do
      Prob.expectation @Double @Double fromIntegral ex1 `shouldBe` (1 / 2 * 3.5 + 1 / 2 * 1.5)

  let outcomes_ex1 =
        [(x, 1 / 12) | x <- [3 .. 6]]
          ++ [(x, 1 / 4 + 1 / 12) | x <- [1, 2]]

  describe "Outcomes" $ do
    it "ex1" $ do
      ex1 `shouldBeDistribution` outcomes_ex1

  describe "transforms" $ do
    it "scales" $ do
      Prob.scale 0.5 ex1 `shouldBeDistribution` (outcomes_ex1 <&> _2 %~ (0.5 *))
    it "conditional" $ do
      Prob.mass (Prob.conditional (<= 2) ex1) `shouldBe` Prob.probabilityOf (<= 2) ex1
      Prob.conditional (<= 2) ex1 `shouldBeDistribution` [(1, 1 / 3), (2, 1 / 3)]
      Prob.postselect (<= 2) ex1 `shouldBeDistribution` [(1, 1 / 2), (2, 1 / 2)]

  describe "expectationA" $ do
    it "support" $ do
      Prob.support ex1 `shouldBe` [1 .. 6] ++ [1 .. 6] ++ [1, 2]

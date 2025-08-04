{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Traq.Data.ProbabilitySpec where

import Data.Foldable (toList)

import Traq.Data.Probability

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import TestHelpers

type Distr' = Distr Double

instance (Arbitrary a) => Arbitrary (Distr' a) where
  arbitrary = sized $ \n -> chooseInt (0, n) >>= go
   where
    go 0 = return $ Branch []
    go 1 = Leaf <$> arbitrary
    go n = do
      ss <- partSubtreeSizes (n - 1)
      ts <- mapM go ss
      ps <- partProb (n - 1) 1
      return $ Branch $ zip ps ts

    partSubtreeSizes :: Int -> Gen [Int]
    partSubtreeSizes 1 = return [1]
    partSubtreeSizes n | n > 1 = do
      first <- chooseInt (1, n)
      rest <- partSubtreeSizes (n - first)
      return $ first : rest
    partSubtreeSizes _ = return []

    partProb :: Int -> Double -> Gen [Double]
    partProb 0 _ = return []
    partProb n p = do
      p0 <- (p *) <$> genDouble
      ps' <- partProb (n - 1) (p - p0)
      return $ p0 : ps'

_splat :: Distr' (a -> b) -> Distr' a -> Distr' b
_splat = (<*>)

_bind :: Distr' Int -> (Int -> Distr' Int) -> Distr' Int
_bind = (>>=)

spec :: Spec
spec = do
  describe "termProb" $ do
    prop "is a sub-probability" $ \t ->
      weight (t :: Distr' ()) `shouldSatisfy` \p -> 0 <= p && p <= 1

    it "examples" $ do
      weight (Leaf ()) `shouldBe` 1.0
      weight (Branch []) `shouldBe` 0.0
      weight (Branch [(0.5, Leaf ())]) `shouldBe` 0.5

  describe "Instance Laws" $ do
    let f = (+ 1) :: Int -> Int
    let g = (* 2) :: Int -> Int

    describe "Foldable" $ do
      let fromList = uniform :: [Int] -> Distr' Int
      prop "toList . fromList = id" $
        toList . fromList `shouldEqual` id

    describe "Functor laws" $ do
      prop "Identity" $
        fmap @Distr' @Int id `shouldEqual` id

      prop "Composition" $
        fmap @Distr' @Int (f . g) `shouldEqual` fmap @Distr' f . fmap @Distr' g

    describe "Applicative laws" $ do
      prop "Identity" $ \v ->
        pure @Distr' id <*> v `shouldBe` (v :: Distr' Int)

      prop "Homomorphism" $ \x ->
        pure @Distr' f <*> pure @Distr' x `shouldBe` pure @Distr' (f (x :: Int))

      prop "Interchange" $ \(ut, y) -> do
        let u = fmap (+) ut
        u `_splat` pure @Distr' (y :: Int) `shouldBe` pure @Distr' ($ y) `_splat` u

      prop "Composition" $ \(ut, vt, w) -> do
        let u = fmap (+) ut :: Distr' (Int -> Int)
        let v = fmap (*) vt :: Distr' (Int -> Int)
        ((pure @Distr' (.) `_splat` u) `_splat` v) `_splat` w
          `shouldBe` u `_splat` (v `_splat` w)

    describe "Monad laws" $ do
      let mf a = uniform [a, a]
      let mg a = Branch $ map (1.0 / 3.0,) [pure a, Branch [], pure a]

      prop "Right Unit" $ \m ->
        m `_bind` return @Distr' `shouldBe` m

      prop "Left Unit" $ \x ->
        return @Distr' x `_bind` mf `shouldBe` mf x

      prop "Associativity" $ \m ->
        (m `_bind` mf) `_bind` mg `shouldBe` m `_bind` (\x -> mf x `_bind` mg)

  describe "compose" $ do
    let coin = uniform [0, 1] :: Distr' Int
    let dice = uniform [1 .. 6] :: Distr' Int

    it ">>=" $ do
      let m = do
            c <- coin
            if c == 0
              then do
                d <- dice
                uniform $ replicate d d
              else coin
      m
        `shouldBe` Branch
          [ (0.5, Branch [(1.0 / 6, uniform (replicate d d)) | d <- [1 .. 6]])
          , (0.5, Branch [(0.5, Leaf 0), (0.5, Leaf 1)])
          ]

    it "mapM" $ do
      let m = mapM (const coin) [(), ()]
      m
        `shouldBe` Branch
          [ (0.5, uniform [[0, 0], [0, 1]])
          , (0.5, uniform [[1, 0], [1, 1]])
          ]

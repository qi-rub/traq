{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Traq.Data.Probability.TreeSpec where

import Control.Applicative
import Data.Foldable (toList)

import Traq.Data.Probability.Class
import Traq.Data.Probability.Tree

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

distrSpec :: Spec
distrSpec = do
  describe "termProb" $ do
    prop "is a sub-probability" $ \t ->
      mass (t :: Distr' ()) `shouldSatisfy` \p -> 0 <= p && p <= 1

    it "examples" $ do
      mass (Leaf ()) `shouldBe` (1.0 :: Double)
      mass (Branch []) `shouldBe` (0.0 :: Double)
      mass (Branch [(0.5, Leaf ())]) `shouldBe` (0.5 :: Double)

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
  describe "support" $ do
    prop "support = toList" $ support @_ @Distr' @Int `shouldEqual` toList

-- | Construct a non-deterministic choice
nondetChoice :: forall a. [Tree a] -> Tree a
nondetChoice [a] = a
nondetChoice as = Branch [((), a) | a <- as]

treeFromList :: forall a. [a] -> Tree a
treeFromList = nondetChoice . map pure

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized $ \n -> chooseInt (0, n) >>= go
   where
    go 0 = return empty
    go 1 = pure <$> arbitrary
    go n = do
      ss <- partSubtreeSizes (n - 1)
      ts <- mapM go ss
      return $ nondetChoice ts

    partSubtreeSizes :: Int -> Gen [Int]
    partSubtreeSizes 1 = return [1]
    partSubtreeSizes n | n > 1 = do
      first <- chooseInt (1, n)
      rest <- partSubtreeSizes (n - first)
      return $ first : rest
    partSubtreeSizes _ = return []

_fmap :: (Int -> Int) -> Tree Int -> Tree Int
_fmap = fmap

_pure :: a -> Tree a
_pure = pure

_splatT :: Tree (a -> b) -> Tree a -> Tree b
_splatT = (<*>)

_return :: Int -> Tree Int
_return = return

_bindT :: Tree Int -> (Int -> Tree Int) -> Tree Int
_bindT = (>>=)

_empty :: Tree Int
_empty = empty

_alt :: Tree Int -> Tree Int -> Tree Int
_alt = (<|>)

treeSpec :: Spec
treeSpec = do
  describe "Instance Laws" $ do
    let f = (+ 1) :: Int -> Int
    let g = (* 2) :: Int -> Int

    describe "Foldable" $ do
      prop "toList . treeFromList = id" $
        toList . (treeFromList @Int) `shouldEqual` id

    describe "Functor laws" $ do
      prop "Identity" $
        _fmap id `shouldEqual` id

      prop "Composition" $
        _fmap (f . g) `shouldEqual` _fmap f . _fmap g

    describe "Applicative laws" $ do
      prop "Identity" $ \v ->
        _pure id <*> v `shouldBe` (v :: Tree Int)

      prop "Homomorphism" $ \x ->
        _pure f <*> _pure x `shouldBe` _pure (f (x :: Int))

      prop "Interchange" $ \(ut, y) -> do
        let u = fmap (+) ut
        u `_splatT` _pure (y :: Int) `shouldBe` _pure ($ y) `_splatT` u

      prop "Composition" $ \(ut, vt, w) -> do
        let u = fmap (+) ut :: Tree (Int -> Int)
        let v = fmap (*) vt :: Tree (Int -> Int)
        ((_pure (.) `_splatT` u) `_splatT` v) `_splatT` w
          `shouldBe` u `_splatT` (v `_splatT` w)

    describe "Monad laws" $ do
      let mf a = treeFromList [a, a]
      let mg a = nondetChoice [pure a, empty, pure a]

      prop "Right Unit" $ \m ->
        m `_bindT` _return `shouldBe` m

      prop "Left Unit" $ \x ->
        _return x `_bindT` mf `shouldBe` mf x

      prop "Associativity" $ \m ->
        (m `_bindT` mf) `_bindT` mg `shouldBe` m `_bindT` (\x -> mf x `_bindT` mg)

    describe "Alternative laws" $ do
      prop "Right Unit" $ \u ->
        _empty `_alt` u `shouldBe` u

      prop "Left Unit" $ \u ->
        u `_alt` _empty `shouldBe` u

      prop "Associativity" $ \(u, v, w) ->
        u `_alt` (v `_alt` w) `shouldBe` (u `_alt` v) `_alt` w

    describe "MonadPlus laws" $ do
      let mf a = nondetChoice [pure a, pure a]
      prop "Left zero" $
        (_empty >>= mf) `shouldBe` _empty

      prop "Right zero" $ \m -> do
        -- TODO this is weaker than the actual law:
        -- ((m :: Tree Int) >> _empty) `shouldBe` _empty
        ((m :: Tree Int) >> _empty) `shouldSatisfy` null

  describe "compose" $ do
    let coin = treeFromList [0, 1] :: Tree Int
    let dice = treeFromList [1 .. 6] :: Tree Int

    it ">>=" $ do
      let m = do
            c <- coin
            if c == 0
              then do
                d <- dice
                nondetChoice $ replicate d (pure d)
              else coin
      m
        `shouldBe` nondetChoice
          [ nondetChoice [nondetChoice (replicate d $ pure d) | d <- [1 .. 6]]
          , coin
          ]

    it "mapM" $ do
      let m = mapM (const coin) [(), ()]
      m
        `shouldBe` nondetChoice
          [ treeFromList [[0, 0], [0, 1]]
          , treeFromList [[1, 0], [1, 1]]
          ]

spec :: Spec
spec = do
  describe "Distr'" distrSpec
  describe "Tree" treeSpec

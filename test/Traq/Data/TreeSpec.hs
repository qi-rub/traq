{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Traq.Data.TreeSpec where

import Control.Applicative
import Data.Foldable (toList)

import Traq.Data.Probability (Distr (..), Tree)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestHelpers

-- | Construct a non-deterministic choice
nondetChoice :: forall a. [Tree a] -> Tree a
nondetChoice [a] = a
nondetChoice as = Branch [((), a) | a <- as]

fromList :: forall a. [a] -> Tree a
fromList = nondetChoice . map pure

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

_splat :: Tree (a -> b) -> Tree a -> Tree b
_splat = (<*>)

_return :: Int -> Tree Int
_return = return

_bind :: Tree Int -> (Int -> Tree Int) -> Tree Int
_bind = (>>=)

_empty :: Tree Int
_empty = empty

_alt :: Tree Int -> Tree Int -> Tree Int
_alt = (<|>)

spec :: Spec
spec = do
  describe "Instance Laws" $ do
    let f = (+ 1) :: Int -> Int
    let g = (* 2) :: Int -> Int

    describe "Foldable" $ do
      prop "toList . fromList = id" $
        toList . (fromList @Int) `shouldEqual` id

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
        u `_splat` _pure (y :: Int) `shouldBe` _pure ($ y) `_splat` u

      prop "Composition" $ \(ut, vt, w) -> do
        let u = fmap (+) ut :: Tree (Int -> Int)
        let v = fmap (*) vt :: Tree (Int -> Int)
        ((_pure (.) `_splat` u) `_splat` v) `_splat` w
          `shouldBe` u `_splat` (v `_splat` w)

    describe "Monad laws" $ do
      let mf a = fromList [a, a]
      let mg a = nondetChoice [pure a, empty, pure a]

      prop "Right Unit" $ \m ->
        m `_bind` _return `shouldBe` m

      prop "Left Unit" $ \x ->
        _return x `_bind` mf `shouldBe` mf x

      prop "Associativity" $ \m ->
        (m `_bind` mf) `_bind` mg `shouldBe` m `_bind` (\x -> mf x `_bind` mg)

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
    let coin = fromList [0, 1] :: Tree Int
    let dice = fromList [1 .. 6] :: Tree Int

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
          [ fromList [[0, 0], [0, 1]]
          , fromList [[1, 0], [1, 1]]
          ]

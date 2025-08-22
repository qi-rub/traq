{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Probability Monad represented by a Continuation of random variables.
Described in: "Stochastic lambda calculus and monads of probability distributions" (https://dl.acm.org/doi/10.1145/565816.503288)
-}
module Traq.Data.Probability.Cont (
  ExpMonad (..),
) where

import Control.Monad.Cont (ContT (..))
import Data.Traversable (for)

import Traq.Data.Probability.Class

newtype ExpMonad probT a = ExpMonad (forall f. (Applicative f) => ContT probT f a)

runExp :: (Applicative f) => ExpMonad probT a -> ContT probT f a
runExp (ExpMonad c) = c

instance Functor (ExpMonad probT) where
  fmap f (ExpMonad a) = ExpMonad $ fmap f a

instance Applicative (ExpMonad probT) where
  pure a = ExpMonad $ pure a
  (ExpMonad f) <*> (ExpMonad a) = ExpMonad $ f <*> a

instance Monad (ExpMonad probT) where
  (ExpMonad a) >>= f = ExpMonad $ a >>= (runExp . f)

instance (Num probT) => MonadProb probT (ExpMonad probT) where
  choose pxs = ExpMonad $ ContT $ \k ->
    sum <$> for pxs (\(p, ExpMonad x) -> (p *) <$> runContT x k)

instance (Num probT) => MonadExp probT (ExpMonad probT) where
  expectationA h (ExpMonad x) = runContT x h

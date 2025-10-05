{-# LANGUAGE DeriveFunctor #-}
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

import qualified Numeric.Algebra as Alg

import Traq.Data.Probability.Class

newtype ExpMonad probT a = ExpMonad (forall f r. (Applicative f, RVType probT r) => ContT r f a)
  deriving (Functor)

runExp :: (Applicative f, RVType probT r) => ExpMonad probT a -> ContT r f a
runExp (ExpMonad c) = c

runExpCont :: (Applicative f, RVType probT r) => ExpMonad probT a -> (a -> f r) -> f r
runExpCont = runContT . runExp

instance Applicative (ExpMonad probT) where
  pure a = ExpMonad $ pure a
  (ExpMonad f) <*> (ExpMonad a) = ExpMonad $ f <*> a

instance Monad (ExpMonad probT) where
  (ExpMonad a) >>= f = ExpMonad $ a >>= (runExp . f)

instance (ProbType probT) => MonadProb probT (ExpMonad probT) where
  choose pxs = ExpMonad $ ContT $ \k ->
    Alg.sum <$> for pxs (\(p, x) -> (p Alg..*) <$> runExpCont x k)

instance (ProbType probT) => MonadExp probT (ExpMonad probT) where
  expectationA = flip runExpCont

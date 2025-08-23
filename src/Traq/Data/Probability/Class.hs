{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Data.Probability.Class (
  -- * Probability Classes
  MonadProb (..),
  MonadExp (..),

  -- * Types
  Event,

  -- * Special Distributions
  zero,
  choose2,
  uniform,
  bernoulli,

  -- * Properties
  expectation,
  mass,
  probabilityOf,

  -- * Transforms
  scale,
  normalize,
  conditional,
  postselect,

  -- * Support Monad
  support,
  toDeterministicValue,
) where

import Control.Applicative (Const (..))
import Control.Monad (guard)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Monoid (Endo (..))

import Traq.Control.Monad

-- | A random event: represented as a boolearn predicate function.
type Event a = a -> Bool

-- | A probability monad, with a zero distribution and a choice operation.
class (Num probT, Monad m) => MonadProb probT m | m -> probT where
  -- | Weighted choice from a given list of distributions.
  choose :: [(probT, m a)] -> m a

instance (Num probT, MonadProb probT m) => MonadProb probT (ReaderT r m) where
  choose ms = ReaderT $ \r -> do
    choose [(p, runReaderT m r) | (p, m) <- ms]

-- | null/zero distribution
zero :: (MonadProb probT m) => m a
zero = choose []

-- | binary weighted choice
choose2 :: (MonadProb probT m) => probT -> m a -> m a -> m a
choose2 p x y = choose [(p, x), (1 - p, y)]

-- | uniform random choice from a list of values
uniform :: (MonadProb probT m, Fractional probT) => [a] -> m a
uniform [] = zero
uniform [x] = pure x
uniform xs =
  let p = 1 / fromIntegral (length xs)
   in choose [(p, pure x) | x <- xs]

-- | Bernoulli random variable.
bernoulli :: (MonadProb probT m) => probT -> m Bool
bernoulli p = choose2 p (pure True) (pure False)

-- | Scale the probabilities by a factor @s@
scale :: (MonadProb probT m) => probT -> m a -> m a
scale p x = choose [(p, x)]

-- | A probability monad with support for computing expectation of random variables.
class (MonadProb probT m) => MonadExp probT m where
  -- | compute the expectation of a random variable, by sequencing the outcomes.
  expectationA :: (Applicative f) => (a -> f probT) -> m a -> f probT

-- | compute the expectation of a random variable.
expectation :: (MonadExp probT m) => (a -> probT) -> m a -> probT
expectation f = runIdentity . expectationA (Identity . f)

-- | Compute the mass or total probability of a given probability distribution.
mass :: (Num probT, MonadExp probT m) => m a -> probT
mass = expectation $ const 1

-- | Probability of a given event.
probabilityOf :: (Num probT, MonadExp probT m) => Event a -> m a -> probT
probabilityOf ev = expectation $ \a -> if ev a then 1 else 0

-- | Normalize a distribution
normalize :: (Fractional probT, Eq probT, MonadExp probT m) => m a -> m a
normalize mu =
  let p = mass mu
   in if p == 0
        then zero
        else scale (1 / p) mu

-- | unnormalized conditional probability distribution
conditional :: (MonadExp probT m, Fractional probT, Eq probT) => Event a -> m a -> m a
conditional ev m = do
  a <- m
  if ev a then pure a else zero

-- | Normalized conditional probability of an event.
postselect :: (MonadExp probT m, Fractional probT, Eq probT) => Event a -> m a -> m a
postselect ev m =
  let p = probabilityOf ev m
   in if p == 0
        then zero
        else scale (1 / p) $ conditional ev m

-- | Support of a distribution
support :: (MonadExp p m) => m a -> [a]
support = (appEndo ?? []) . getConst . expectationA (Const . Endo . (:))

toDeterministicValue :: (MonadExp p m, Eq a) => m a -> Maybe a
toDeterministicValue m = do
  (x : xs) <- return $ support m
  guard $ all (== x) xs
  return x

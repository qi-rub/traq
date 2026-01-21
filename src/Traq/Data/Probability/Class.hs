{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Data.Probability.Class (
  -- * Probability Classes
  MonadProb (..),
  MonadExp (..),

  -- * Types
  Event,
  ProbType,
  RandVar,
  RVType,

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
  outcomes,
  toDeterministicValue,
) where

import Control.Applicative (Const (..))
import Control.Monad (guard)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..))
import Data.List (nub)
import Data.Monoid (Endo (..))

import qualified Numeric.Algebra as Alg

import Traq.Control.Monad

import Traq.Numeric.Floating ()

-- | A random event: represented as a boolearn predicate function.
type Event a = a -> Bool

-- | Representing a probability (usually double).
type ProbType probT = (Num probT, Alg.Monoidal probT)

-- | A probability monad, with a zero distribution and a choice operation.
class (ProbType probT, Monad m) => MonadProb probT m | m -> probT where
  -- | Weighted choice from a given list of distributions.
  choose :: (ProbType probT) => [(probT, m a)] -> m a

instance (ProbType probT, MonadProb probT m) => MonadProb probT (ReaderT r m) where
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

type RandVar a r = a -> r

type RVType probT r = (Alg.Monoidal r, Alg.Module probT r)

-- | A probability monad with support for computing expectation of random variables.
class (MonadProb probT m) => MonadExp probT m where
  -- | compute the expectation of a random variable, by sequencing the outcomes.
  expectationA :: forall f r a. (Applicative f, RVType probT r) => (a -> f r) -> m a -> f r

-- | compute the expectation of a random variable.
expectation :: forall probT r m a. (MonadExp probT m, RVType probT r) => RandVar a r -> m a -> r
expectation f = runIdentity . expectationA (Identity . f)

-- | Compute the mass or total probability of a given probability distribution.
mass :: (Num probT, MonadExp probT m, RVType probT probT) => m a -> probT
mass = expectation $ const 1

-- | Probability of a given event.
probabilityOf :: forall probT m a. (MonadExp probT m, RVType probT probT) => Event a -> m a -> probT
probabilityOf ev = expectation $ \a -> if ev a then 1 else 0

-- | Normalize a distribution
normalize :: (Fractional probT, Eq probT, MonadExp probT m, RVType probT probT) => m a -> m a
normalize mu =
  let p = mass mu
   in if p == 0
        then zero
        else scale (1 / p) mu

-- | unnormalized conditional probability distribution
conditional :: (MonadExp probT m, Fractional probT, Eq probT, RVType probT probT) => Event a -> m a -> m a
conditional ev m = do
  a <- m
  if ev a then pure a else zero

-- | Normalized conditional probability of an event.
postselect :: (MonadExp probT m, Fractional probT, Eq probT, RVType probT probT) => Event a -> m a -> m a
postselect ev m =
  let p = probabilityOf ev m
   in if p == 0
        then zero
        else scale (1 / p) $ conditional ev m

-- | Support of a distribution
support :: forall p m a. (MonadExp p m, Alg.Module p ()) => m a -> [a]
support = (appEndo ?? []) . getConst . (expectationA @_ @_ @_ @()) (\a -> Const (Endo (a :)))

-- | outcomes
outcomes :: (MonadExp p m, Eq a, RVType p p, Alg.Module p ()) => m a -> [(a, p)]
outcomes mu = do
  a <- nub $ support mu
  return (a, probabilityOf (== a) mu)

toDeterministicValue :: (MonadExp p m, Eq a, Alg.Module p ()) => m a -> Maybe a
toDeterministicValue m = do
  (x : xs) <- return $ support m
  guard $ all (== x) xs
  return x

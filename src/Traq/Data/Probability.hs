{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Probabilistic computation monad.
module Traq.Data.Probability (
  -- * Probability Monad
  Distr (..),

  -- ** Properties
  weight,
  outcomes,
  deterministicValue,
  fromDiracDelta,

  -- ** Transformers
  flatten,
  fullFlatten,

  -- ** Special case constructors
  uniform,
  choice,
  Tree,

  -- * Monad Transformer
  ProbT (..),
  Prob,
  runProb,

  -- * Monad Class
  MonadProb (..),
) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((>>>))
import Control.Monad (MonadPlus (..), guard, join)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import Lens.Micro.GHC

type Outcomes probT a = [(probT, a)]

compressOutcomes :: (Num probT, Eq a, Ord a) => Outcomes probT a -> Outcomes probT a
compressOutcomes =
  sortOn snd
    >>> groupOn snd
    >>> map (\t -> (sum . map fst $ t, snd . head $ t))

{- | A tree representation of a probability distributions.
E.g. @Distr Double a@ for real-valued probabilities.
@Distr () a@ for non-deterministic value.
-}
data Distr probT a
  = -- | A deterministic value (probability 1)
    Leaf a
  | -- | A probabilistic choice with at least two outcomes
    Branch [(probT, Distr probT a)]
  deriving (Show, Read, Eq, Ord)

instance Foldable (Distr probT) where
  foldr f b (Leaf a) = f a b
  foldr f b (Branch pts) = foldr (flip (foldr f)) b $ map snd pts

instance Traversable (Distr probT) where
  traverse focus (Leaf a) = Leaf <$> focus a
  traverse focus (Branch pts) = Branch <$> (traverse . _2 . traverse) focus pts

instance Functor (Distr probT) where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch pts) = Branch [(p, fmap f t) | (p, t) <- pts]

instance Applicative (Distr probT) where
  pure = Leaf

  (Leaf f) <*> t = fmap f t
  (Branch pfs) <*> t = Branch [(p, f <*> t) | (p, f) <- pfs]

instance Monad (Distr probT) where
  (Leaf a) >>= mb = mb a
  (Branch pts) >>= mb = Branch [(p, t >>= mb) | (p, t) <- pts]

instance MonadFail (Distr probT) where
  fail _ = Branch []

-- | Total probability mass of a given sub-distribution.
weight :: (Num probT) => Distr probT a -> probT
weight (Leaf _) = 1
weight (Branch ts) = sum [p * weight t | (p, t) <- ts]

-- | A flat list of pairs of probability and outcome
outcomes :: (Num probT) => Distr probT a -> [(probT, a)]
outcomes = go 1
 where
  go p (Leaf a) = [(p, a)]
  go p (Branch ts) = concat [go (p * pt) t | (pt, t) <- ts]

-- | Construct a distribution given a list of samples with probabilities.
fromOutcomes :: [(probT, a)] -> Distr probT a
fromOutcomes = Branch . map (second Leaf)

flattenWith :: (Num probT, Eq a, Ord a) => (Outcomes probT a -> Outcomes probT a) -> Distr probT a -> Distr probT a
flattenWith f = fromOutcomes . f . outcomes

-- | Flatten a probability tree to one level.
flatten :: (Num probT) => Distr probT a -> Distr probT a
flatten = fromOutcomes . outcomes

-- | Flatten a probability tree to one level, and merge equal outcomes.
fullFlatten :: (Num probT, Eq a, Ord a) => Distr probT a -> Distr probT a
fullFlatten = flattenWith compressOutcomes

-- | Convert a distribution into a single value, and raise an error if not a delta-distribution.
deterministicValue :: (Eq a, Foldable f) => f a -> Maybe a
deterministicValue t = do
  (x : xs) <- pure $ toList t
  guard $ all (== x) xs
  return x

-- | Convert a dirac-delta distribution to a value.
fromDiracDelta :: (Eq a) => Distr probT a -> Maybe a
fromDiracDelta = deterministicValue

type Tree = Distr ()

instance Alternative Tree where
  empty = Branch []

  t1 <|> t2 = from_l $ to_l t1 ++ to_l t2
   where
    from_l [a] = a
    from_l as = Branch $ map ((),) as

    to_l (Branch ts) = map snd ts
    to_l t = [t]

instance MonadPlus Tree where
  mzero = empty
  mplus = (<|>)

-- | Probability monad transformer
newtype ProbT probT m a = ProbT {runProbT :: m (Distr probT a)}

instance (Functor m) => Functor (ProbT probT m) where
  fmap f = ProbT . fmap (fmap f) . runProbT

instance (Applicative m) => Applicative (ProbT probT m) where
  pure = ProbT . pure . pure

  (ProbT mtf) <*> (ProbT mta) = let mft = fmap (<*>) mtf in ProbT $ mft <*> mta

instance (Monad m) => Monad (ProbT probT m) where
  (ProbT mta) >>= f = ProbT $ mta >>= traverse (runProbT . f) <&> join

instance MonadTrans (ProbT probT) where
  lift = ProbT . fmap pure

type Prob probT = ProbT probT Identity

runProb :: Prob probT a -> Distr probT a
runProb = runIdentity . runProbT

-- | mtl-style class for probability monads
class (Monad m) => MonadProb probT m | m -> probT where
  -- | weighted choice over a given set of monadic actions
  choiceM :: [(probT, m a)] -> m a

-- | Construct a uniform probabilistic choice
uniform :: (Fractional probT, MonadProb probT m, Foldable f) => f a -> m a
uniform ms = choiceM . map ((p,) . pure) . toList $ ms
 where
  n = length ms
  p = if n == 0 then 1.0 else 1.0 / fromIntegral n

-- | Biased binary choice
choice :: (MonadProb probT m, Num probT) => probT -> a -> a -> m a
choice p x y = choiceM [(p, pure x), (1 - p, pure y)]

instance MonadProb probT (Distr probT) where
  choiceM = Branch

instance (Monad m) => MonadProb probT (ProbT probT m) where
  choiceM =
    unzip
      >>> second (mapM runProbT)
      >>> sequence
      >>> fmap (uncurry zip)
      >>> fmap choiceM
      >>> ProbT

instance (MonadProb probT m) => MonadProb probT (ReaderT r m) where
  choiceM =
    unzip
      >>> second (mapM runReaderT)
      >>> sequence
      >>> fmap (uncurry zip)
      >>> fmap choiceM
      >>> ReaderT

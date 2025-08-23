{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

-- | Probabilistic computation monad.
module Traq.Data.Probability.Tree (
  -- * Tree of outcomes representation of a probability distribution.
  Distr (..),
  flattenAndCompress,

  -- * Non-deterministic monad
  Tree,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Bifunctor (second)
import Data.Foldable (toList)

import Lens.Micro.GHC

import Traq.Data.Probability.Class
import Traq.Data.Probability.List

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

instance (Num probT) => MonadProb probT (Distr probT) where
  choose = Branch

instance (Num probT) => MonadExp probT (Distr probT) where
  expectationA rv (Leaf a) = rv a
  expectationA rv (Branch pts) = sum <$> sequenceA [(p *) <$> expectationA rv t | (p, t) <- pts]

treeToProbList :: (Num probT) => Distr probT a -> ProbList probT a
treeToProbList = ProbList . go 1
 where
  go p (Leaf a) = [(p, a)]
  go p (Branch ts) = concat [go (p * pt) t | (pt, t) <- ts]

treeFromProbList :: (Num probT) => ProbList probT a -> Distr probT a
treeFromProbList = Branch . map (second Leaf) . runProbList

-- | Flatten a probability tree to one level, and merge equal outcomes.
flattenAndCompress :: (Num probT, Ord a) => Distr probT a -> Distr probT a
flattenAndCompress = treeFromProbList . compress . treeToProbList

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

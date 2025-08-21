{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Data.Probability.List (
  ProbList (..),
  Outcomes,
  compress,
) where

import Control.Arrow ((>>>))
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.Extra (groupOn)

import Traq.Data.Probability.Class

type Outcomes probT a = [(probT, a)]

newtype ProbList probT a = ProbList {runProbList :: Outcomes probT a}
  deriving (Eq, Read, Show, Functor, Foldable)

instance (Num probT) => Applicative (ProbList probT) where
  pure x = ProbList [(1, x)]

  (ProbList fs) <*> (ProbList xs) =
    ProbList
      [ (pf * px, f x)
      | (pf, f) <- fs
      , (px, x) <- xs
      ]

instance (Num probT) => Monad (ProbList probT) where
  (ProbList xs) >>= f =
    ProbList
      [ (px * py, y)
      | (px, x) <- xs
      , (py, y) <- runProbList (f x)
      ]

instance (Num probT) => MonadProb probT (ProbList probT) where
  choose pls = ProbList [(pl * px, x) | (pl, l) <- pls, (px, x) <- runProbList l]

instance (Num probT) => MonadExp probT (ProbList probT) where
  expectationA rv (ProbList pxs) = sum <$> sequenceA [(p *) <$> rv x | (p, x) <- pxs]

instance MonadSupport (ProbList probT) where
  support = toList

compress :: (Num probT, Ord a) => ProbList probT a -> ProbList probT a
compress =
  runProbList
    >>> sortOn snd
    >>> groupOn snd
    >>> map (\t -> (sum . map fst $ t, snd . head $ t))
    >>> ProbList

-- | Support for non-determinism with a Tree data structure.
module QCompose.Utils.Tree (
  Tree (..),
  fromList,
  flattenSingle,
  termProb,
) where

import Control.Applicative
import Control.Monad

-- | A representation of a non-deterministic value
data Tree a
  = -- | A deterministic value
    Leaf a
  | -- | A non-deterministic choice (without priority)
    Choice [Tree a]
  deriving (Show, Read, Eq, Ord)

-- Convert a list to a single 'Choice'
fromList :: [a] -> Tree a
fromList = Choice . map Leaf

-- Drop all 'Choice' nodes with a single child
flattenSingle :: Tree a -> Tree a
flattenSingle (Choice [a]) = flattenSingle a
flattenSingle (Choice as) = Choice $ map flattenSingle as
flattenSingle a = a

{- | Probability that the computation terminates.
 'Choice' is treated as a uniform random choice.
-}
termProb :: Tree a -> Float
termProb (Leaf _) = 1.0
termProb (Choice []) = 0.0
termProb (Choice as) =
  sum (map termProb as) / fromIntegral (length as)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Choice ts) = foldr (flip (foldr f)) b ts

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Choice as) = Choice (map (fmap f) as)

instance Applicative Tree where
  pure = Leaf

  (Leaf f) <*> a = fmap f a
  (Choice fs) <*> a = Choice $ map (<*> a) fs

instance Monad Tree where
  (Leaf a) >>= mb = mb a
  (Choice as) >>= mb = Choice $ map (>>= mb) as

instance Alternative Tree where
  empty = Choice []

  (Choice []) <|> b = b
  a <|> (Choice []) = a
  (Choice as) <|> (Choice bs) = Choice (as ++ bs)
  (Choice as) <|> (Leaf b) = Choice (as ++ [Leaf b])
  (Leaf a) <|> (Choice bs) = Choice (Leaf a : bs)
  (Leaf a) <|> (Leaf b) = Choice [Leaf a, Leaf b]

instance MonadPlus Tree where
  mzero = empty
  mplus = (<|>)

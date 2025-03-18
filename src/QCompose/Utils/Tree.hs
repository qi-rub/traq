-- | Support for non-determinism with a Tree data structure.
module QCompose.Utils.Tree (
  Tree (..),
  choice,
  flattenTrivial,
  termProb,
) where

import Control.Applicative
import Control.Monad

-- | A representation of a non-deterministic value
data Tree a
  = -- | Failed computation (i.e. 0 choices)
    Fail
  | -- | A deterministic value (i.e. 1 choice)
    Leaf a
  | -- | A non-deterministic choice with a non-trivial branch (i.e. at least two outcomes)
    Choice (Tree a) (Tree a) [Tree a]
  deriving (Show, Read, Eq, Ord)

-- | Construct a non-deterministic choice
choice :: [Tree a] -> Tree a
choice [] = Fail
choice [a] = a
choice (a : a' : as) = Choice a a' as

-- flatten all trivial branches
flattenTrivial :: Tree a -> Tree a
flattenTrivial (Choice a a' as) = go (flattenTrivial a) (flattenTrivial a') (map flattenTrivial as)
 where
  isFail :: Tree a -> Bool
  isFail Fail = True
  isFail _ = False

  go Fail Fail bs | all isFail bs = Fail
  go b b' bs = Choice b b' bs
flattenTrivial a = a

{- | Probability that the computation terminates.
 'Choice' is treated as a uniform random choice.
-}
termProb :: Tree a -> Float
termProb Fail = 0.0
termProb (Leaf _) = 1.0
termProb (Choice a a' as) =
  sum (map termProb (a : a' : as)) / fromIntegral (2 + length as)

instance Foldable Tree where
  foldr _ b Fail = b
  foldr f b (Leaf a) = f a b
  foldr f b (Choice t t' ts) = foldr (flip (foldr f)) b (t : t' : ts)

instance Functor Tree where
  fmap _ Fail = Fail
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Choice a a' as) =
    Choice (fmap f a) (fmap f a') (map (fmap f) as)

instance Applicative Tree where
  pure = Leaf

  Fail <*> _ = Fail
  (Leaf f) <*> a = fmap f a
  (Choice f f' fs) <*> a =
    Choice (f <*> a) (f' <*> a) $ map (<*> a) fs

instance Monad Tree where
  Fail >>= _ = Fail
  (Leaf a) >>= mb = mb a
  (Choice a a' as) >>= mb =
    Choice (a >>= mb) (a' >>= mb) $ map (>>= mb) as

instance Alternative Tree where
  empty = Fail

  Fail <|> b = b
  a <|> Fail = a
  (Choice a a' as) <|> (Choice b b' bs) = Choice a a' (as ++ (b : b' : bs))
  (Choice a a' as) <|> (Leaf b) = Choice a a' (as ++ [Leaf b])
  (Leaf a) <|> (Choice b b' bs) = Choice (Leaf a) b (b' : bs)
  (Leaf a) <|> (Leaf b) = Choice (Leaf a) (Leaf b) []

instance MonadPlus Tree where
  mzero = empty
  mplus = (<|>)

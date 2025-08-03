{-# OPTIONS_GHC -Wno-orphans #-}

-- | Support for non-determinism with a Tree data structure.
module Traq.Data.Tree (
  Tree,
  choice,
  detExtract,
) where

import Data.Foldable (toList)

import Traq.Data.Probability (Tree, nondetChoice)

-- | Construct a non-deterministic choice
choice :: [Tree a] -> Tree a
choice = nondetChoice

detExtract :: (Foldable t) => t a -> a
detExtract xs = case toList xs of
  [x] -> x
  _ -> error "unexpected non-determinism"

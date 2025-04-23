{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives.Search.Prelude (
  HasSearch (..),
) where

import QCompose.Prelude

class HasSearch primT where
  mkAny :: Ident -> primT
  mkSearch :: Ident -> primT

  getPredicate :: primT -> Ident
  returnsSol :: primT -> Bool

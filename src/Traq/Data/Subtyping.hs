{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Data.Subtyping (
  (:<:) (..),
  IsA (..),
) where

import GHC.Generics

-- | Subtyping for has-a relationships.
class sub :<: sup where
  -- | Inject a subtype into its supertype.
  inject :: sub -> sup
  default inject :: (Generic sup, GHasA sub (Rep sup)) => sub -> sup
  inject = to . ginject

  -- | Project out a subtype value from the supertype.
  project :: sup -> Maybe sub
  default project :: (Generic sup, GHasA sub (Rep sup)) => sup -> Maybe sub
  project = gproject . from

class GHasA sub fsup where
  ginject :: sub -> fsup a
  gproject :: fsup a -> Maybe sub

instance (GHasA sub fsup) => GHasA sub (M1 i c fsup) where
  ginject = M1 . ginject
  gproject (M1 x) = gproject x

instance (sub :<: fsup) => GHasA sub (K1 i fsup) where
  ginject = K1 . inject
  gproject (K1 x) = project x

instance a :<: a where
  inject = id
  project = Just

-- | Subtyping for is-a relationships.
class IsA t a where
  extract :: a -> t
  default extract :: (Generic a, GIsA t (Rep a)) => a -> t
  extract = gextract . from

class GIsA t f where
  gextract :: f a -> t

instance (GIsA t f1, GIsA t f2) => GIsA t (f1 :+: f2) where
  gextract (L1 x) = gextract x
  gextract (R1 x) = gextract x

instance (GIsA t f) => GIsA t (M1 i c f) where
  gextract (M1 x) = gextract x

instance (IsA t a) => GIsA t (K1 i a) where
  gextract (K1 x) = extract x

instance IsA a a where extract = id

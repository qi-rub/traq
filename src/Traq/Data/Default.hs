{-# LANGUAGE DefaultSignatures #-}

module Traq.Data.Default (HasDefault (..)) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics

-- | Simple class for types with a default value.
class HasDefault a where
  default_ :: a
  default default_ :: (Generic a, GHasDefault (Rep a)) => a
  default_ = to gdefault

-- | Generic for @HasDefault@
class GHasDefault f where
  gdefault :: f a

instance GHasDefault U1 where
  gdefault = U1

instance (GHasDefault a, GHasDefault b) => GHasDefault (a :*: b) where
  gdefault = gdefault :*: gdefault

instance (GHasDefault a) => GHasDefault (M1 i c a) where
  gdefault = M1 gdefault

instance (HasDefault a) => GHasDefault (K1 i a) where
  gdefault = K1 default_

-- Basic Instances
instance HasDefault [a] where
  default_ = []

instance HasDefault (Map.Map k v) where
  default_ = Map.empty

instance HasDefault (Set.Set v) where
  default_ = Set.empty

-- Derived Instances
instance (HasDefault a, HasDefault b) => HasDefault (a, b)
instance (HasDefault a, HasDefault b, HasDefault c) => HasDefault (a, b, c)
instance (HasDefault a, HasDefault b, HasDefault c, HasDefault d) => HasDefault (a, b, c, d)

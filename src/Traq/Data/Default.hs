module Traq.Data.Default (HasDefault (..)) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Simple class for types with a default value.
class HasDefault a where
  default_ :: a

instance (HasDefault a, HasDefault b) => HasDefault (a, b) where
  default_ = (default_, default_)

instance (HasDefault a, HasDefault b, HasDefault c) => HasDefault (a, b, c) where
  default_ = (default_, default_, default_)

instance (HasDefault a, HasDefault b, HasDefault c, HasDefault d) => HasDefault (a, b, c, d) where
  default_ = (default_, default_, default_, default_)

instance HasDefault [a] where
  default_ = []

instance HasDefault (Map.Map k v) where
  default_ = Map.empty

instance HasDefault (Set.Set v) where
  default_ = Set.empty

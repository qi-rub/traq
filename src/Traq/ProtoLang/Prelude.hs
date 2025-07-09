module Traq.ProtoLang.Prelude (
  SizeType,
  CostType,
  PrimitiveType,

  -- * Meta-parameters
  MetaParam (..),
) where

import Traq.Prelude
import qualified Traq.Utils.Printing as PP

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The cost type (usually some Floating) of a given type
type family CostType p

-- | The bag of primitives used by a given type
type family PrimitiveType p

-- | Compile-time constant parameters
data MetaParam sizeT = MetaName String | MetaSize sizeT | MetaValue Value
  deriving (Eq, Show, Read)

instance (Show sizeT) => PP.ToCodeString (MetaParam sizeT) where
  toCodeString (MetaName n) = "#" ++ n
  toCodeString (MetaSize n) = show n
  toCodeString (MetaValue n) = show n

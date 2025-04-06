module QCompose.Primitives where

import qualified Data.Map as Map
import QCompose.Prelude
import QCompose.Primitives.Class

-- | A mapping from primitive names to their concrete implementation.
type PrimitivesMap sizeT costT = Map.Map Ident (PrimitiveImpl sizeT costT)

-- | Default primitives, used by the examples.
defaultPrimitives :: PrimitivesMap sizeT costT
defaultPrimitives = Map.empty

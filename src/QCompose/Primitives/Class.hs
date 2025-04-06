module QCompose.Primitives.Class (
  PrimitiveImpl (..),
) where

{- | Implementation of a primitive.
 Provides the abstract cost formulas, typechecker, evaluator, and lowerings.
-}
data PrimitiveImpl sizeT costT = PrimitiveImpl

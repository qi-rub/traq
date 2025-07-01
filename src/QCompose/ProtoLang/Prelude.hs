module QCompose.ProtoLang.Prelude (
  SizeType,
  CostType,
  PrimitiveType,
) where

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The cost type (usually some Floating) of a given type
type family CostType p

-- | The bag of primitives used by a given type
type family PrimitiveType p

module Traq.Prelude where

import qualified Traq.Data.Symbolic as Sym

type Ident = String

type SizeT = Int

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

type Value = Integer

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The cost type (usually some Floating) of a given type
type family CostType p

-- | The bag of primitives used by a given type
type family PrimitiveType p

-- | The type of statement holes (yet-to-implement) in a language
type family HoleType p

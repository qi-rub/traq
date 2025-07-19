module Traq.Prelude where

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

type Ident = String

type SizeT = Int

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The cost type (usually some Floating) of a given type
type family CostType p

-- | The bag of primitives used by a given type
type family PrimitiveType p

-- | The type of statement holes (yet-to-implement) in a language
type family HoleType p

type instance SizeType (Ctx.Context e) = SizeType e
type instance CostType (Ctx.Context e) = CostType e
type instance PrimitiveType (Ctx.Context e) = PrimitiveType e
type instance HoleType (Ctx.Context e) = HoleType e

type instance SizeType [e] = SizeType e
type instance CostType [e] = CostType e
type instance PrimitiveType [e] = PrimitiveType e
type instance HoleType [e] = HoleType e

{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Prelude where

import qualified Traq.Data.Context as Ctx

type Ident = String

type SizeT = Int

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The precision type (usually some Floating) of a given type.
type family PrecType p

-- | The bag of primitives used by a given type
type family PrimitiveType p

type instance SizeType (Ctx.Context e) = SizeType e
type instance PrecType (Ctx.Context e) = PrecType e
type instance PrimitiveType (Ctx.Context e) = PrimitiveType e

type instance SizeType [e] = SizeType e
type instance PrecType [e] = PrecType e
type instance PrimitiveType [e] = PrimitiveType e

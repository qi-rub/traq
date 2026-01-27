{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Prelude where

import qualified Data.Map as Map

import qualified Traq.Data.Context as Ctx

type Ident = String

type SizeT = Int

-- | The size type (usually some Integral) of a given type.
type family SizeType p

-- | The precision type (usually some Floating) of a given type.
type family PrecType p

type instance SizeType (Ctx.Context e) = SizeType e
type instance PrecType (Ctx.Context e) = PrecType e

type instance SizeType (Map.Map k v) = SizeType v
type instance PrecType (Map.Map k v) = PrecType v

type instance SizeType [e] = SizeType e
type instance PrecType [e] = PrecType e

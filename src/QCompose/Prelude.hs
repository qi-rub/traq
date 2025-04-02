module QCompose.Prelude where

import qualified Data.Number.Symbolic as Sym

type Ident = String

type SizeT = Int

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

type Value = Integer

boolToValue :: Bool -> Value
boolToValue True = 1
boolToValue False = 0

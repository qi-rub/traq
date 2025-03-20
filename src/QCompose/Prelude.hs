module QCompose.Prelude where

import qualified Data.Number.Symbolic as Sym

type Ident = String

type Value = Integer
type SizeT = Int

type FailProb = Float
type Precision = Float

boolToValue :: Bool -> Value
boolToValue True = 1
boolToValue False = 0

-- | Value type for representing the query complexity.
type Complexity = Float

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

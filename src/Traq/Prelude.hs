module Traq.Prelude where

import qualified Traq.Data.Symbolic as Sym

type Ident = String

type SizeT = Int

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

type Value = Integer

module QCompose.Basic where

type Ident = String

type Value = Integer
type SizeT = Int

type FailProb = Float
type Precision = Float

data Symbolic a = SymExpr String | Value a deriving (Eq)
type SymbSize = Symbolic SizeT

instance (Show a) => Show (Symbolic a) where
  show (SymExpr e) = e
  show (Value a) = show a

boolToValue :: Bool -> Value
boolToValue True = 1
boolToValue False = 0

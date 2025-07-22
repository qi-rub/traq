{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map as Map
import Text.Parsec.String (parseFromFile)
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import qualified Traq.ProtoLang as P

import Traq.Primitives.Search.DetSearch (DetSearch (..))
import Traq.Primitives.Search.QSearchCFNW (QSearchCFNW (..))
import Traq.Primitives.Search.RandomSearch (RandomSearch (..))

type Matrix = SizeT -> SizeT -> Bool

matrixToFun :: Matrix -> [P.Value SizeT] -> [P.Value SizeT]
matrixToFun matrix [P.FinV i, P.FinV j] = [P.boolToValue $ matrix i j]
matrixToFun _ _ = error "invalid indices"

expectedCost ::
  forall primT.
  ( P.CanParsePrimitive primT
  , P.QuantumCostablePrimitive primT primT SizeT Double
  ) =>
  Int ->
  Int ->
  Matrix ->
  Double ->
  IO Double
expectedCost n m matrix eps = do
  -- load the program
  Right loaded_program <- parseFromFile (P.programParser @primT) "examples/matrix_search/matrix_search.qb"
  let program = Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n) <$> loaded_program

  -- cost of each _unitary_ call to Matrix
  let uticks = Map.singleton "Matrix" 1
  -- cost of each _classical_ call to Matrix
  let cticks = Map.singleton "Matrix" 1

  -- the functionality of Matrix, provided as input data
  let interp = Ctx.singleton "Matrix" (matrixToFun matrix)

  return $
    P.quantumQueryCost @primT
      P.SplitUsingNeedsEps -- precision splitting strategy
      eps -- maximum failure probability
      program
      uticks
      cticks
      interp
      mempty

main :: IO ()
main = do
  putStrLn "Demo: Matrix Search"

  let (n, m) = (1000, 1000)
  let sample_matrix i j = j /= m - 1
  let eps = 0.01

  putStrLn "Costs for sample matrix:"

  putStr "  Quantum      : "
  print =<< expectedCost @QSearchCFNW n m sample_matrix eps
  putStr "  Deterministic: "
  print =<< expectedCost @DetSearch n m sample_matrix eps
  putStr "  Randomized   : "
  print =<< expectedCost @RandomSearch n m sample_matrix eps

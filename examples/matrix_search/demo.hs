{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec.String (parseFromFile)
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Prelude
import qualified Traq.ProtoLang as P

import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Primitives (Primitive)
import Traq.Primitives.Search.DetSearch (DetSearch (..))
import Traq.Primitives.Search.QSearchCFNW (QSearchCFNW (..))
import Traq.Primitives.Search.RandomSearch (RandomSearch (..))

type Matrix = SizeT -> SizeT -> Bool

matrixToFun :: Matrix -> [P.Value SizeT] -> [P.Value SizeT]
matrixToFun matrix [P.FinV i, P.FinV j] = [P.toValue $ matrix i j]
matrixToFun _ _ = error "invalid indices"

expectedCost ::
  forall primT primT'.
  ( P.Parseable primT'
  , A.AnnotateWithErrorBudgetU primT
  , A.AnnotateWithErrorBudgetQ primT
  , A.ExpCostQ (A.AnnFailProb primT) SizeT Double
  , SizeType primT' ~ Sym.Sym Int
  , P.MapSize primT'
  , primT ~ P.MappedSize primT' Int
  , primT' ~ P.MappedSize primT (Sym.Sym Int)
  ) =>
  Int ->
  Int ->
  Matrix ->
  Double ->
  IO Double
expectedCost n m matrix eps = do
  -- load the program
  Right loaded_program <- parseFromFile (P.programParser @primT') "examples/matrix_search/matrix_search.traq"
  let program = P.mapSize (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n)) loaded_program
  program_annotated <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) program

  -- the functionality of Matrix, provided as input data
  let interp = Ctx.singleton "Matrix" (matrixToFun matrix)

  return $ getCost $ A.expCostQProg program_annotated mempty interp

main :: IO ()
main = do
  putStrLn "Demo: Matrix Search"

  let (n, m) = (500, 500)
  let sample_matrix _i j = j /= m - 1
  let eps = 0.001

  putStrLn "Costs for sample matrix:"

  putStr "  Quantum      : "
  print =<< expectedCost @(Primitive (QSearchCFNW _ _)) n m sample_matrix eps
  putStr "  Deterministic: "
  print =<< expectedCost @(Primitive (DetSearch _ _)) n m sample_matrix eps
  putStr "  Randomized   : "
  print =<< expectedCost @(Primitive (RandomSearch _ _)) n m sample_matrix eps

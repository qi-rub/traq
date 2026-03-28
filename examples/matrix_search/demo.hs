{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Options.Applicative
import Text.Parsec.String (parseFromFile)
import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost
import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives (DefaultPrims, Primitive)
import Traq.Primitives.Search.DetSearch (DetSearch (..))
import Traq.Primitives.Search.QSearchCFNW (QSearchCFNW (..))
import Traq.Primitives.Search.RandomSearch (RandomSearch (..))

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

data Options = Options
  { optCompareCosts :: Bool
  , optDemo :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch (long "compare-costs" <> short 'c' <> help "Run cost comparison")
    <*> switch (long "demo" <> short 'd' <> help "Run demo analysis")

type Matrix = SizeT -> SizeT -> Bool

matrixToFun :: Matrix -> [CPL.Value SizeT] -> [CPL.Value SizeT]
matrixToFun matrix [CPL.FinV i, CPL.FinV j] = [CPL.toValue $ matrix i j]
matrixToFun _ _ = error "invalid indices"

expectedCost ::
  forall primT primT'.
  ( CPL.Parseable primT'
  , A.AnnotateWithErrorBudgetU primT
  , A.AnnotateWithErrorBudgetQ primT
  , A.ExpCostQ (A.AnnFailProb primT) SizeT Double
  , SizeType primT' ~ Sym.Sym Int
  , CPL.MapSize primT'
  , primT ~ CPL.MappedSize primT' Int
  , primT' ~ CPL.MappedSize primT (Sym.Sym Int)
  ) =>
  Int ->
  Int ->
  Matrix ->
  Double ->
  IO Double
expectedCost n m matrix eps = do
  -- load the program
  Right loaded_program <- parseFromFile (CPL.programParser @primT') "examples/matrix_search/matrix_search.traq"
  let program = CPL.mapSize (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n)) loaded_program
  program_annotated <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) program

  -- the functionality of Matrix, provided as input data
  let interp = mempty & at "Matrix" ?~ matrixToFun matrix

  return $ getCost $ A.expCostQProg program_annotated mempty interp

compareCosts :: IO ()
compareCosts = do
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

demo :: IO ()
demo = do
  let (n, m) = (20, 10)
  let eps = 0.001

  Right loaded_program <- parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym Int) Double)) "examples/matrix_search/matrix_search.traq"
  let prog = CPL.mapSize (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n)) loaded_program
  prog_ann <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) prog

  let sample_matrix i j = i <= j
  let interp = mempty & at "Matrix" ?~ matrixToFun sample_matrix

  putStrLn $ "tvErrorQ  : " ++ show (A.tvErrorQProg prog_ann)
  putStrLn $ "costU     : " ++ show (A.costUProg prog_ann :: QueryCost Double)
  putStrLn $ "costQ     : " ++ show (A.costQProg prog_ann :: QueryCost Double)
  putStrLn $ "expCostQ  : " ++ show (A.expCostQProg prog_ann mempty interp :: QueryCost Double)

main :: IO ()
main = do
  opts <- execParser optsInfo
  when (optDemo opts) demo
  when (optCompareCosts opts) compareCosts
 where
  optsInfo =
    info
      (optionsParser <**> helper)
      (fullDesc <> progDesc "Matrix Search Demo")

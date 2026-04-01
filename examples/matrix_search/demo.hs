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
import qualified Traq.Data.Symbolic as Sym
import Traq.Prelude
import Traq.Primitives (DefaultPrims, Primitive)
import Traq.Primitives.Search.DetSearch (DetSearch (..))
import Traq.Primitives.Search.QSearchCFNW (QSearchCFNW (..))
import Traq.Primitives.Search.RandomSearch (RandomSearch (..))

import Lens.Micro.GHC

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
  forall primT'.
  ( CPL.Parseable primT'
  , SizeType primT' ~ Sym.Sym Int
  , PrecType primT' ~ Sym.Sym Double
  , CPL.MapPrec primT'
  , CPL.MapSize (CPL.MappedPrec primT' Double)
  , SizeType (CPL.MappedPrec primT' Double) ~ Sym.Sym Int
  , PrecType (CPL.MappedPrec primT' Double) ~ Double
  , A.AnnotateWithErrorBudgetU (CPL.MappedSize (CPL.MappedPrec primT' Double) Int)
  , A.AnnotateWithErrorBudgetQ (CPL.MappedSize (CPL.MappedPrec primT' Double) Int)
  , A.ExpCostQ (A.AnnFailProb (CPL.MappedSize (CPL.MappedPrec primT' Double) Int)) SizeT Double
  ) =>
  Int ->
  Int ->
  Matrix ->
  Double ->
  IO Double
expectedCost n m matrix eps = do
  -- load the program
  Right loaded_program <- parseFromFile (CPL.programParser @primT') "examples/matrix_search/matrix_search.traq"
  let program = CPL.mapSize (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n)) $ CPL.mapPrec Sym.unSym loaded_program
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
  print =<< expectedCost @(Primitive (QSearchCFNW (Sym.Sym Int) (Sym.Sym Double))) n m sample_matrix eps
  putStr "  Deterministic: "
  print =<< expectedCost @(Primitive (DetSearch (Sym.Sym Int) (Sym.Sym Double))) n m sample_matrix eps
  putStr "  Randomized   : "
  print =<< expectedCost @(Primitive (RandomSearch (Sym.Sym Int) (Sym.Sym Double))) n m sample_matrix eps

demo :: IO ()
demo = do
  let (n, m) = (20, 10)
  let eps = 0.001

  Right loaded_program <- parseFromFile (CPL.programParser @(DefaultPrims (Sym.Sym Int) (Sym.Sym Double))) "examples/matrix_search/matrix_search.traq"
  let prog = CPL.mapSize (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n)) $ CPL.mapPrec Sym.unSym loaded_program
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

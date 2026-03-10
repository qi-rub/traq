{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec.String (parseFromFile)
import qualified Traq.Data.Symbolic as Sym

import Lens.Micro.GHC
import qualified Traq.Analysis as A
import Traq.Prelude
import qualified Traq.CPL as CPL

import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Primitives (Primitive)
import Traq.Primitives.Amplify.CAmplify (CAmplify (..))
import Traq.Primitives.Amplify.QAmplify (QAmplify (..))
import qualified Traq.Utils.Printing as PP

type Matrix = SizeT -> SizeT -> Bool

listToFun :: [SizeT] -> [CPL.Value SizeT] -> [CPL.Value SizeT]
listToFun xs [CPL.FinV i] = [CPL.toValue $ xs !! i]
listToFun _ _ = error "invalid index"

matrixToFun :: Matrix -> [CPL.Value SizeT] -> [CPL.Value SizeT]
matrixToFun matrix [CPL.FinV i, CPL.FinV j] = [CPL.toValue $ matrix i j]
matrixToFun _ _ = error "invalid indices"

data Ctx = Ctx
  { n :: Int
  , num_iter :: Int
  , capacity :: Int
  , profits, weights :: [Int]
  }

substCtx :: Ctx -> Sym.Sym Int -> Int
substCtx Ctx{..} =
  Sym.unSym
    . Sym.subst "N" (Sym.con n)
    . Sym.subst "K" (Sym.con num_iter)
    . Sym.subst "W" (Sym.con 1000)
    . Sym.subst "P" (Sym.con 1000)

worstCaseCost
  , expectedCost ::
    forall primT primT'.
    ( CPL.Parseable primT'
    , A.AnnotateWithErrorBudgetU primT
    , A.AnnotateWithErrorBudgetQ primT
    , A.ExpCostQ (A.AnnFailProb primT) SizeT Double
    , SizeType primT' ~ Sym.Sym Int
    , CPL.MapSize primT'
    , primT ~ CPL.MappedSize primT' Int
    , primT' ~ CPL.MappedSize primT (Sym.Sym Int)
    , PP.ToCodeString primT
    ) =>
    Ctx ->
    Double ->
    IO Double
-- worst case cost (ignores data)
worstCaseCost ctx eps = do
  -- load the program
  loaded_program <- either (fail . show) pure =<< parseFromFile (CPL.programParser @primT') "examples/tree_generator/tree_generator_01_knapsack.traq"
  let program = CPL.mapSize (substCtx ctx) loaded_program
  program_annotated <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) program

  return $ getCost $ A.costQProg program_annotated
-- expected cost (depends on data)
expectedCost ctx@Ctx{..} eps = do
  -- load the program
  loaded_program <- either (fail . show) pure =<< parseFromFile (CPL.programParser @primT') "examples/tree_generator/tree_generator_01_knapsack.traq"
  let program = CPL.mapSize (substCtx ctx) loaded_program
  -- putStrLn $ replicate 80 '='
  -- putStrLn $ PP.toCodeString program
  -- putStrLn $ replicate 80 '='
  program_annotated <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) program
  -- putStrLn $ replicate 80 '='
  -- putStrLn $ PP.toCodeString program_annotated
  -- putStrLn $ replicate 80 '='

  -- the functionality of Matrix, provided as input data
  let interp =
        mempty
          & (at "Capacity" ?~ \_ -> [CPL.toValue capacity])
          & (at "Profit" ?~ listToFun profits)
          & (at "Weight" ?~ listToFun weights)

  return $ getCost $ A.expCostQProg program_annotated mempty interp

main :: IO ()
main = do
  putStrLn "Demo: Matrix Search"

  let ctx =
        Ctx
          { n = 3
          , capacity = 10
          , profits = [1, 2, 3, 4, 5]
          , weights = [2, 2, 1, 1, 1]
          , num_iter = 1
          }
  let eps = 0.005

  putStrLn "Costs for sample 0-1 knapsack instance:"

  putStr "  Quantum (worst-case):   "
  print =<< worstCaseCost @(Primitive (QAmplify _ _)) ctx eps
  putStr "  Classical (worst-case): "
  print =<< worstCaseCost @(Primitive (CAmplify _ _)) ctx eps
  putStr "  Quantum (expected):     "
  print =<< expectedCost @(Primitive (QAmplify _ _)) ctx eps
  putStr "  Classical (expected):   "
  print =<< expectedCost @(Primitive (CAmplify _ _)) ctx eps

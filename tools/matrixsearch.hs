{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (assert)
import Control.Monad (forM_)
import Data.List (inits)
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.CostModel.QueryCost (QueryCost (..))
import Traq.Examples.MatrixSearch
import Traq.Prelude
import Traq.Primitives.Search.Symbolic
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

symbolicEx :: IO ()
symbolicEx = do
  putStrLn "Symbolic program:"

  let n = Sym.var "N" :: Sym.Sym SizeT
  let m = Sym.var "M" :: Sym.Sym SizeT
  let P.Program ex_fs = matrixExample @QSearchSym n m
  let strat = P.SplitUsingNeedsEps

  forM_ (tail $ inits ex_fs) $ \fs -> do
    let eps = P.failProb (Sym.var "ε" :: Sym.Sym Double)
    let delta = P.l2NormError (Sym.var "δ" :: Sym.Sym Double)

    putStrLn $ printf "Worst case cost of %s" (P.fun_name $ last fs)
    putStr "  - Unitary: "
    print (P.unitaryQueryCost @_ @(Sym.Sym SizeT) @(Sym.Sym Double) @_ strat delta (P.Program fs) :: QueryCost (Sym.Sym Double))
    putStr "  - Quantum: "
    print (P.quantumMaxQueryCost strat eps (P.Program fs) :: QueryCost (Sym.Sym Double))

concreteEx :: IO ()
concreteEx = do
  putStrLn "Concrete program:"
  -- let (n, m) = (1000, 1000)
  let (n, m) = (20, 10)
  let ex = matrixExampleS n m

  printDivider
  putStrLn $ PP.toCodeString ex

  let delta = P.l2NormError (0.001 :: Double)
  let strat = P.SplitUsingNeedsEps

  let u_formula_cost = P.unitaryQueryCost strat delta ex :: QueryCost Double

  printDivider
  Right exU <- return $ CompileU.lowerProgram strat Ctx.empty delta ex
  putStrLn $ PP.toCodeString exU

  let (u_true_cost, _) = CQPL.programCost exU

  putStrLn "Unitary Cost:"
  putStrLn $ " - Abstract cost: " <> show u_formula_cost
  putStrLn $ " - Actual cost:   " <> show u_true_cost
  assert (u_true_cost == u_formula_cost) $ return ()

concreteQEx :: IO ()
concreteQEx = do
  putStrLn "Concrete program (quantum):"
  -- let (n, m) = (1000, 1000)
  let (n, m) = (20, 10)
  let ex = matrixExampleS n m

  printDivider
  putStrLn $ PP.toCodeString ex

  let eps = P.failProb (0.001 :: Double)
  let strat = P.SplitUsingNeedsEps

  printDivider
  Right exU <- return $ CompileQ.lowerProgram strat Ctx.empty eps ex
  putStrLn $ PP.toCodeString exU
  return ()

main :: IO ()
main = do
  putStrLn "hello qcompose"

  printDivider
  symbolicEx
  printDivider

  printDivider
  concreteEx
  printDivider

  printDivider
  concreteQEx
  printDivider

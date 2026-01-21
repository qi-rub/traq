module Main where

import Control.Exception (assert)
import Control.Monad (forM_)
import Data.List (inits)
import Text.Printf (printf)

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.Analysis as P
import Traq.Analysis.CostModel.QueryCost (QueryCost (..))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Examples.MatrixSearch
import Traq.Prelude
import Traq.Primitives.Class (Primitive (..))
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.Symbolic
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

type P = Primitive (QSearchSym SizeT Double)

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

symbolicEx :: IO ()
symbolicEx = do
  putStrLn "Symbolic program:"

  let n = Sym.var "N" :: Sym.Sym SizeT
  let m = Sym.var "M" :: Sym.Sym SizeT
  let P.Program ex_fs = mkMatrixExample (\t f -> P.PrimCallE $ Primitive [f] $ QSearchSym $ PrimSearch AnyK t) n m

  forM_ (tail $ inits ex_fs) $ \fs -> do
    let eps = A.failProb (Sym.var "ε" :: Sym.Sym Double)

    putStrLn $ printf "Worst case cost of %s" (P.fun_name $ last fs)

    putStr "  - Unitary: "
    prog_u <- either fail pure $ A.annotateProgWithErrorBudgetU eps (P.Program fs)
    print (A.costUProg prog_u :: QueryCost (Sym.Sym Double))

    putStr "  - Quantum: "
    prog_q <- either fail pure $ A.annotateProgWithErrorBudget eps (P.Program fs)
    print (A.costQProg prog_q :: QueryCost (Sym.Sym Double))

concreteEx :: IO ()
concreteEx = do
  putStrLn "Concrete program:"
  -- let (n, m) = (1000, 1000)
  let (n, m) = (20, 10)
  let ex = matrixExampleS n m

  printDivider
  putStrLn $ PP.toCodeString ex

  let eps = P.failProb (0.001 :: Double)
  ex' <- either fail pure $ A.annotateProgWithErrorBudget eps ex

  let u_formula_cost = P.costUProg ex' :: QueryCost Double

  printDivider
  Right exU <- return $ Compiler.lowerProgramU ex'
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
  ex' <- either fail pure $ A.annotateProgWithErrorBudget eps ex

  printDivider
  Right exU <- return $ Compiler.lowerProgram ex'
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

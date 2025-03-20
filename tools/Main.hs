module Main where

import qualified Data.Map as M

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

import Control.Exception (assert)
import QCompose.Examples.MatrixSearch
import QCompose.Primitives.QSearch
import QCompose.Utils.Printing

main :: IO ()
main = do
  putStrLn "hello qcompose"

  let (n, m) = (10, 10)
  let ex = matrixExampleS n m

  putStrLn $ replicate 80 '='
  putStrLn $ toCodeString ex

  let delta = 0.001

  let u_formula_cost = P.unitaryQueryCost cadeEtAlFormulas delta ex

  putStrLn $ replicate 80 '='
  let (Right (exU, _)) = U.lowerProgram zalkaQSearch M.empty delta ex
  putStrLn $ toCodeString exU

  let u_true_cost = U.programCost exU

  putStrLn "Unitary Cost:"
  putStrLn $ " - Abstract cost: " <> show u_formula_cost
  putStrLn $ " - Actual cost: " <> show u_true_cost
  assert (u_true_cost == u_formula_cost) $ return ()

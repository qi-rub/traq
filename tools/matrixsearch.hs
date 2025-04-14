module Main where

import Control.Exception (assert)
import qualified Data.Number.Symbolic as Sym
import Lens.Micro

import qualified QCompose.Data.Context as Ctx

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

import QCompose.Examples.MatrixSearch
import QCompose.Prelude
import QCompose.Primitives.QSearch
import QCompose.Utils.Printing

symbolicEx :: IO ()
symbolicEx = do
  putStrLn "symbolic program:"

  let n = Sym.var "N" :: Sym.Sym SizeT
  let m = Sym.var "M" :: Sym.Sym SizeT
  let ex = matrixExample n m (P.Fin $ Sym.con 2)

  let delta = Sym.var "δ" :: Sym.Sym Double
  let u_formula_cost = (qsearchCFNWSymbolic ^. to formulas . to P.unitaryQueryCost) delta ex "Oracle"
  print u_formula_cost

concreteEx :: IO ()
concreteEx = do
  putStrLn "concrete program:"
  let (n, m) = (1000, 1000)
  let ex = matrixExampleS n m

  putStrLn $ replicate 80 '='
  putStrLn $ toCodeString ex

  let delta = 0.001 :: Double

  let u_formula_cost = P.unitaryQueryCost (qsearchCFNW ^. to formulas) delta ex "Oracle"

  putStrLn $ replicate 80 '='
  let (Right (exU, _)) = UQPL.lowerProgram (qsearchCFNW ^. to unitaryAlgo) Ctx.empty "Oracle" delta ex
  putStrLn $ toCodeString exU

  let (u_true_cost, _) = UQPL.programCost (\hole -> error "TODO") exU

  putStrLn "Unitary Cost:"
  putStrLn $ " - Abstract cost: " <> show u_formula_cost
  putStrLn $ " - Actual cost:   " <> show u_true_cost
  assert (u_true_cost == u_formula_cost) $ return ()

main :: IO ()
main = do
  putStrLn "hello qcompose"
  symbolicEx
  concreteEx

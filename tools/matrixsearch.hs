{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (assert)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Symbolic as Sym

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Examples.MatrixSearch
import QCompose.Primitives.Search.Symbolic

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

symbolicEx :: IO ()
symbolicEx = do
  putStrLn "Symbolic program:"

  let n = Sym.var "N" :: Sym.Sym SizeT
  let m = Sym.var "M" :: Sym.Sym SizeT
  let ex = matrixExample @QSearchSym n m (P.Fin $ Sym.con 2)

  forM_ ["HasAllOnesRow", "IsRowAllOnes", "IsEntryZero"] $ \f -> do
    let delta = Sym.var "δ" :: Sym.Sym Double
    let stmt =
          P.ExprS
            { rets = undefined
            , expr =
                P.FunCallE
                  { P.fun_kind = P.FunctionCall f
                  , P.args = undefined
                  }
            }

    putStrLn $ printf "Cost of %s" f
    print $ P.unitaryQueryCost delta ex{P.stmt = stmt} (Map.singleton "Oracle" 1.0)

concreteEx :: IO ()
concreteEx = do
  putStrLn "Concrete program:"
  -- let (n, m) = (1000, 1000)
  let (n, m) = (20, 10)
  let ex = matrixExampleS n m

  printDivider
  putStrLn $ toCodeString ex

  let delta = 0.001 :: Double
  let uticks = Map.singleton "Oracle" 1.0

  let u_formula_cost = P.unitaryQueryCost delta ex uticks

  printDivider
  let (Right (exU, _)) = UQPL.lowerProgram Ctx.empty uticks delta ex
  putStrLn $ toCodeString exU

  let (u_true_cost, _) = UQPL.programCost exU

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
  putStrLn $ toCodeString ex

  let delta = 0.001 :: Double
  let ticks = Map.singleton "Oracle" 1.0

  printDivider
  let (Right (exU, _)) = CQPL.lowerProgram Ctx.empty ticks delta ex
  putStrLn $ toCodeString exU

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

{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (assert)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Compiler.Quantum as CQPL
import qualified Traq.Compiler.Unitary as UQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import qualified Traq.Utils.Printing as PP

import Traq.Examples.MatrixSearch
import Traq.Primitives.Search.Symbolic

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

symbolicEx :: IO ()
symbolicEx = do
  putStrLn "Symbolic program:"

  let n = Sym.var "N" :: Sym.Sym SizeT
  let m = Sym.var "M" :: Sym.Sym SizeT
  let ex = matrixExample @QSearchSym n m (P.Fin $ Sym.con 2)
  let strat = P.SplitUsingNeedsEps
  let uticks = Map.singleton "Oracle" (Sym.var "c_u")
  let cticks = Map.singleton "Oracle" (Sym.var "c_c")

  forM_ ["HasAllOnesRow", "IsRowAllOnes", "IsEntryZero"] $ \f -> do
    let stmt =
          P.ExprS
            { P.rets = undefined
            , P.expr =
                P.FunCallE
                  { P.fun_kind = P.FunctionCall f
                  , P.args = undefined
                  }
            }

    let eps = Sym.var "ε" :: Sym.Sym Double
    let delta = Sym.var "δ" :: Sym.Sym Double

    putStrLn $ printf "Worst case cost of %s" f
    putStr "  - Unitary: "
    print $ P.unitaryQueryCost strat delta ex{P.stmt = stmt} uticks
    putStr "  - Quantum: "
    print $ P.quantumMaxQueryCost strat eps ex{P.stmt = stmt} uticks cticks

concreteEx :: IO ()
concreteEx = do
  putStrLn "Concrete program:"
  -- let (n, m) = (1000, 1000)
  let (n, m) = (20, 10)
  let ex = matrixExampleS n m

  printDivider
  putStrLn $ PP.toCodeString ex

  let delta = 0.001 :: Double
  let uticks = Map.singleton "Oracle" 1.0
  let strat = P.SplitUsingNeedsEps

  let u_formula_cost = P.unitaryQueryCost strat delta ex uticks

  printDivider
  Right exU <- return $ UQPL.lowerProgram strat Ctx.empty uticks delta ex
  putStrLn $ PP.toCodeString exU

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
  putStrLn $ PP.toCodeString ex

  let delta = 0.001 :: Double
  let uticks = Map.singleton "Oracle" 1.0
  let cticks = Map.singleton "Oracle" 1.0
  let strat = P.SplitUsingNeedsEps

  printDivider
  Right exU <- return $ CQPL.lowerProgram strat Ctx.empty uticks cticks delta ex
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

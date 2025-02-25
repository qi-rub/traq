module Main where

import Control.Monad.State (execStateT)
import qualified Data.Map as M
import QCompose.Examples.MatrixSearch
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.TypeCheck
import QCompose.UnitaryQPL.Lowering
import QCompose.Utils.Printing

showProg :: IO ()
showProg = do
  putStrLn "Matrix Example"
  let ex = matrixExampleS 10 10

  putStr "Is well typed? "
  print $ isWellTyped ex

  let oracleF = \[i, j] -> [if i == j then 1 else 0]
  let res = execStateT (evalProgram ex oracleF) M.empty

  case res of
    Left e -> do
      putStrLn "Error executing program:"
      putStrLn e
    Right v -> do
      putStr "Output: "
      print v

  let eps = 0.0001
  putStr "Query Cost (quantum): "
  print $ quantumQueryCost Quantum cadeEtAlFormulas oracleF ex eps M.empty
  putStr "Query Cost (unitary): "
  print $ quantumQueryCost Unitary cadeEtAlFormulas oracleF ex eps M.empty

  putStrLn "program:"
  putStrLn $ toCodeString ex

  putStrLn "lowering to UQPL:"
  putStrLn $ toCodeString $ lowerProgramU 0.001 ex

main :: IO ()
main = do
  showProg

module Main where

import qualified Data.Map as M
import QCompose.Examples.MatrixSearch
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Printer
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.TypeCheck

showProg :: IO ()
showProg = do
  putStrLn "Matrix Example"
  let ex = matrixExample 10 10

  putStr "Is well typed? "
  print $ isWellTyped ex

  let oracleF = \[i, j] -> [if i == j then 1 else 0]
  let res = evalProgram ex oracleF M.empty

  putStr "Output: "
  print res
  let eps = 0.0001

  putStr "Query Cost: "
  print $ quantumQueryCostOfFun Quantum cadeEtAlFormulas (funCtx ex) oracleF [] eps "check_matrix"

  putStrLn "program:"
  putStrLn $ toCodeString ex

main :: IO ()
main = do
  showProg

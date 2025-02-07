module Main where

import QCompose.Examples.MatrixSearch
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.TypeCheck

showProg :: IO ()
showProg = do
  let ex = matrixExample 10 10
  print $ isWellTyped ex

  -- pPrint $ ex
  let oracleF = \[i, j] -> [if i == j then 1 else 0]
  let res = evalFun ex oracleF [] "check_matrix"
  print res
  let eps = 0.0001
  print $ quantumQueryCostOfFun Quantum cadeEtAlFormulas ex oracleF [] eps "check_matrix"

main :: IO ()
main = do
  showProg

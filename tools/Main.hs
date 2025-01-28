module Main where

import QCompose.ProtoLang
import QCompose.TypeCheck
import QCompose.Examples.MatrixSearch

show_prog :: IO ()
show_prog = do
  let ex = matrixExample 10 10
  print $ typeCheckProg ex (OracleDecl [Fin 10, Fin 10] [Fin 2])

  -- pPrint $ ex
  let oracle = \[i, j] -> [if i == j then 1 else 0]
  let res = evalFun ex oracle [] "check_matrix"
  print res
  let eps = 0.0001
  print $ quantumQueryCostOfFun Quantum cadeEtAlFormulas ex oracle [] eps "check_matrix" 

main :: IO ()
main = do
  show_prog



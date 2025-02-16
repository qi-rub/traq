module Main where

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
  let res = evalFun ex oracleF [] "check_matrix"

  putStr "Output: "
  print res
  let eps = 0.0001

  putStr "Query Cost: "
  print $ quantumQueryCostOfFun Quantum cadeEtAlFormulas ex oracleF [] eps "check_matrix"

  putStrLn "program:"
  putStrLn $
    toCodeString
      ( Program
          { funCtx = ex
          , body = SFunCall{fun = "check_matrix", rets = ["ok"], args = []}
          }
      )

main :: IO ()
main = do
  showProg

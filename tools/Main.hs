{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import QCompose.Basic
import QCompose.Amplify
import QCompose.ProtoLang
import Text.Pretty.Simple
import qualified Data.Map as M
import QCompose.Examples.MatrixSearch

show_prog :: IO ()
show_prog = do
  let ex = matrixExample 10 10
  -- pPrint $ ex
  let oracle = \[i, j] -> [if i == j then 1 else 0]
  let res = evalFun ex oracle [] "check_matrix"
  print res
  let eps = 0.0001
  print $ quantumQueryCostOfFun Quantum cadeEtAlFormulas ex oracle [] eps "check_matrix" 

main :: IO ()
main = do
  show_prog



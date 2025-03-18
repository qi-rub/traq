module Main where

import qualified Data.Map as M

-- import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

import QCompose.Examples.MatrixSearch
import QCompose.Subroutines.QSearch
import QCompose.Utils.Printing

main :: IO ()
main = do
  putStrLn "hello qcompose"

  let (n, m) = (10, 10)
  let ex = matrixExampleS n m

  putStrLn $ replicate 80 '='
  putStrLn $ toCodeString ex

  putStrLn $ replicate 80 '='
  let (Right (exU, _)) = U.lowerProgram zalkaQSearch M.empty 0.001 ex
  putStrLn $ toCodeString exU

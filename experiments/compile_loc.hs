{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Text.Parsec.String (parseFromFile)
import Text.Printf (printf)
import qualified Traq.Analysis as P
import qualified Traq.Analysis as Traq
import qualified Traq.Compiler.Quantum
import qualified Traq.Data.Symbolic as Sym
import Traq.Prelude
import qualified Traq.Primitives as Traq
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

loadProgramFromFile :: String -> IO (P.Program (Traq.DefaultPrims (Sym.Sym SizeT) Double))
loadProgramFromFile fname = do
  sprog_or_err <- parseFromFile (P.programParser @(Traq.DefaultPrims (Sym.Sym SizeT) Double)) fname
  sprog <- either (error . show) pure sprog_or_err
  let sprog' = P.renameVars "" sprog
  -- putStrLn $ PP.toCodeString sprog
  -- putStrLn $ PP.toCodeString sprog'
  return sprog'

subst :: [(String, SizeT)] -> P.Program (Traq.DefaultPrims (Sym.Sym SizeT) Double) -> P.Program (Traq.DefaultPrims SizeT Double)
subst vs p = P.mapSize Sym.unSym $ foldl substOne p vs
 where
  substOne p' (x, v) = P.mapSize (Sym.subst x (Sym.con v)) p'

compileIt :: (ext ~ (Traq.DefaultPrims SizeT Double)) => P.Program ext -> Double -> Either String String
compileIt prog eps = do
  compiled_prog <- Traq.Compiler.Quantum.lowerProgram P.SplitSimple (Traq.failProb eps) prog
  return $ PP.toCodeString compiled_prog

data ExptConfig = ExptConfig
  { expt_name :: String
  , fname :: String
  , params :: [(String, Int)]
  , precision :: Double
  , show_qpl :: Bool
  }

runExpt :: ExptConfig -> IO ()
runExpt ExptConfig{..} = do
  putStrLn $ "\n" ++ expt_name ++ ":"
  sprog <- loadProgramFromFile fname
  let prog = subst params sprog
  qpl <- either fail pure $ compileIt prog precision
  putStrLn $ printf "  LOC: %d" (length $ lines qpl)
  when show_qpl $ putStrLn qpl

matSearch :: Int -> Double -> IO ()
matSearch n eps =
  runExpt
    ExptConfig
      { expt_name = "Matrix Search"
      , fname = "examples/matrix_search/matrix_search.qb"
      , params = [("N", n), ("M", n)]
      , precision = eps
      , show_qpl = False
      }

listSearch :: Int -> Double -> IO ()
listSearch n eps =
  runExpt
    ExptConfig
      { expt_name = "List Search"
      , fname = "examples/primitives/search.qb"
      , params = [("N", n)]
      , precision = eps
      , show_qpl = False
      }

main :: IO ()
main = do
  putStrLn "Experiment: Compilation to QPL"
  matSearch 1000 0.001
  listSearch 1000 0.001

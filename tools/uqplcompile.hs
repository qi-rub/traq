module Main where

import qualified Data.Map as Map

import qualified Data.Number.Symbolic as Sym
import QCompose.Prelude
import QCompose.Primitives.QSearch
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

subsNM :: SizeT -> SizeT -> (SymbSize -> SizeT)
subsNM n m = Sym.unSym . Sym.subst "N" (Sym.con n) . Sym.subst "M" (Sym.con m)

main :: IO ()
main = do
  code <- readFile "examples/matrix_search/matrix_search.qb"
  let n = 20
  let m = 10
  let Right prog = fmap (subsNM n m) <$> P.parseProgram code
  let delta = 0.001
  let Right (uqpl_prog, _) = UQPL.lowerProgram zalkaQSearch Map.empty delta prog
  putStrLn $ toCodeString uqpl_prog

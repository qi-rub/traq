module Main where

import Control.Monad (forM_)
import qualified Data.Number.Symbolic as Sym
import Lens.Micro

import qualified QCompose.Utils.Context as Ctx

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
  let Right (uqpl_prog, _) = UQPL.lowerProgram zalkaQSearch Ctx.empty delta prog
  let (cost, proc_costs) = UQPL.programCost uqpl_prog
  forM_ (uqpl_prog ^. to UQPL.proc_defs) $ \p -> do
    putStrLn $ "// Cost: " <> show (proc_costs ^. at (p ^. to UQPL.proc_name) . singular _Just)
    putStrLn $ toCodeString p

  putStrLn $ "// Cost: " <> show cost
  putStrLn $ toCodeString $ uqpl_prog ^. to UQPL.stmt

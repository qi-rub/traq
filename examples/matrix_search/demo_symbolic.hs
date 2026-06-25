{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Parsec.String (parseFromFile)

import Lens.Micro.GHC

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost
import qualified Traq.CPL as CPL
import qualified Traq.Data.Symbolic as Sym
import Traq.Prelude
import Traq.Primitives (DefaultPrims)

matrixToFun :: (SizeT -> SizeT -> Bool) -> [CPL.Value SizeT] -> [CPL.Value SizeT]
matrixToFun matrix [CPL.FinV i, CPL.FinV j] = [CPL.toValue $ matrix i j]
matrixToFun _ _ = error "invalid indices"

{- | Load matrix_search.traq, substitute concrete sizes N=n and M=m, then
attach a fresh symbolic eps_i to each primitive call. Print the resulting
symbolic error-budget constraint and the three cost analyses as raw Sym
Double ASTs, so the output can be fed to an external optimizer.
-}
dumpSymbolic :: SizeT -> SizeT -> (SizeT -> SizeT -> Bool) -> IO ()
dumpSymbolic n m sample_matrix = do
  Right loaded_program <-
    parseFromFile
      (CPL.programParser @(DefaultPrims (Sym.Sym Int) (Sym.Sym Double)))
      "examples/matrix_search/matrix_search.traq"

  let prog =
        CPL.mapSize
          (Sym.unSym . Sym.subst "M" (Sym.con m) . Sym.subst "N" (Sym.con n))
          loaded_program

  prog_ann <- either fail pure $ A.annSymEpsProg prog

  let interp = mempty & at "Matrix" ?~ matrixToFun sample_matrix

  let tvErr = Sym.simpl $ A.getFailProb $ A.tvErrorQProg prog_ann
  let costU_ = fmap Sym.simpl (A.costUProg prog_ann :: QueryCost (Sym.Sym Double))
  let costQ_ = fmap Sym.simpl (A.costQProg prog_ann :: QueryCost (Sym.Sym Double))
  let expCostQ_ = fmap Sym.simpl (A.expCostQProg prog_ann mempty interp :: QueryCost (Sym.Sym Double))

  putStrLn "# Symbolic epsilon extraction for matrix_search"
  putStrLn $ "#   parameters: N=" ++ show n ++ ", M=" ++ show m
  putStrLn ""
  putStrLn "## Error-budget constraint (tvErrorQ)"
  putStrLn "# Solver constraint: the following expression <= user-chosen budget."
  print tvErr
  putStrLn ""
  putStrLn "## costU (worst-case unitary cost, per external fn)"
  print costU_
  putStrLn ""
  putStrLn "## costQ (worst-case quantum cost, per external fn)"
  print costQ_
  putStrLn ""
  putStrLn "## expCostQ (expected quantum cost, per external fn, data-dependent)"
  putStrLn "# Uses sample_matrix: (\\i j -> i <= j)"
  print expCostQ_

main :: IO ()
main = dumpSymbolic 20 10 (\i j -> i <= j)

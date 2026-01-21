{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM_, replicateM, when)
import Lens.Micro.GHC
import System.Random (randomIO)
import System.TimeIt (timeItT)
import Text.Parsec.String (parseFromFile)
import Text.Printf (printf)
import qualified Traq.Analysis as Traq
import qualified Traq.Analysis.CostModel.QueryCost as Traq
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym
import Traq.Examples.MatrixSearch (matrixExampleS)
import Traq.Prelude
import qualified Traq.Primitives as Traq
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

loadProgramFromFile :: String -> IO (P.Program (Traq.DefaultPrims (Sym.Sym SizeT) Double))
loadProgramFromFile fname = do
  sprog_or_err <- parseFromFile (P.programParser @(Traq.DefaultPrims (Sym.Sym SizeT) Double)) fname
  sprog <- either (error . show) pure sprog_or_err
  let sprog' = P.renameVars "" sprog
  when show_prog $ do
    putStrLn $ PP.toCodeString sprog
    putStrLn $ PP.toCodeString sprog'
  return sprog'
 where
  show_prog = False

type ValidExt ext =
  ( PrecType ext ~ Double
  , SizeType ext ~ SizeT
  , P.HasFreeVars ext
  , Traq.AnnotateWithErrorBudgetU ext
  , Traq.AnnotateWithErrorBudgetQ ext
  , Traq.ExpCostQ (Traq.AnnFailProb ext) SizeT Double
  , Traq.Compiler.CompileQ (Traq.AnnFailProb ext)
  )

-- | Compute the number of qubits used by the compiled program.
numQubitsRequired :: (ValidExt ext) => P.Program ext -> Double -> Either String SizeT
numQubitsRequired prog eps = do
  prog' <- Traq.annotateProgWithErrorBudget (Traq.failProb eps) prog
  compiled_prog <- Traq.Compiler.lowerProgram prog'
  return $ CQPL.numQubits compiled_prog

-- | Compute the wall-time by Traq to run a cost analysis
traqWallTime :: (ValidExt ext) => P.Program ext -> Double -> P.FunInterpCtx SizeT -> IO Double
traqWallTime prog eps fs = do
  (t, _) <- timeItT $ do
    prog' <- either fail pure $ Traq.annotateProgWithErrorBudget (Traq.failProb eps) prog
    let cost :: Traq.SimpleQueryCost Double
        !cost = Traq.expCostQProg prog' [] fs
    return $ Traq.getCost cost
  return t

data ExptResult = ExptResult {wallTime :: Double, numQubits :: SizeT}
  deriving (Show)

runExpt :: (ValidExt ext) => P.Program ext -> Double -> P.FunInterpCtx SizeT -> IO ExptResult
runExpt prog eps fs = do
  wallTime <- traqWallTime prog eps fs
  numQubits <- either fail pure $ numQubitsRequired prog eps
  return ExptResult{..}

runExpt' :: P.Program (Traq.DefaultPrims SizeT Double) -> Double -> P.FunInterpCtx SizeT -> IO ExptResult
runExpt' = runExpt

matrixSearchExpt :: IO ()
matrixSearchExpt = do
  putStrLn "\nMatrix Search:"
  let eps = 0.001 :: Double
  let ns = [10, 20 .. 100] ++ [200, 300 .. 1000] ++ [1400, 1500, 1600]
  putStrLn "n, time, qubits"
  forM_ ns $ \n -> do
    let prog = matrixExampleS n n
    let mat = \case
          [P.FinV i, P.FinV j] -> [P.FinV $ if i == j then 1 else 0]
          _ -> undefined
    ExptResult{wallTime, numQubits} <- runExpt' prog eps (Ctx.singleton "Matrix" mat)
    putStrLn $ printf "%d, %.5f, %d" n wallTime numQubits

depth3NAND :: IO ()
depth3NAND = do
  putStrLn "\nDepth 3 NAND Tree:"
  sprog <- loadProgramFromFile "examples/matrix_search/depth3_NAND_formula.qb"
  let eps = 0.001 :: Double
  let ns = [10, 20 .. 120]
  putStrLn "n, time, qubits"
  forM_ ns $ \n -> do
    let prog =
          sprog
            & P.mapSize
              ( Sym.unSym
                  . Sym.subst "N" (Sym.con n)
                  . Sym.subst "M" (Sym.con n)
                  . Sym.subst "K" (Sym.con n)
              )
    let f = \case
          [P.FinV i, P.FinV j, P.FinV k] -> [P.FinV $ if i == j || j == k then 1 else 0]
          _ -> undefined
    ExptResult{wallTime, numQubits} <- runExpt' prog eps (Ctx.singleton "f" f)
    putStrLn $ printf "%d, %.5f, %d" n wallTime numQubits

hillClimbExpt :: IO ()
hillClimbExpt = do
  putStrLn "\nHillclimb:"
  sprog <- loadProgramFromFile "examples/hillclimb/max_sat_hillclimb.qb"
  let eps = 0.001 :: Double
  let ns = [10, 20 .. 100] ++ [200, 300 .. 1000]
  putStrLn "n, time, qubits"
  forM_ ns $ \n -> do
    let prog = P.mapSize (Sym.unSym . Sym.subst "n" (Sym.con n) . Sym.subst "W" 100) sprog

    -- compute the weight of an assignment
    let phi _ = [P.FinV 0]

    ExptResult{wallTime, numQubits} <- runExpt' prog eps (Ctx.singleton "Phi" phi)
    putStrLn $ printf "%d, %.5f, %d" n wallTime numQubits

triangleFinding :: IO ()
triangleFinding = do
  putStrLn "\nTriangle Finding:"
  sprog <- loadProgramFromFile "examples/triangle_finding.qb"
  let eps = 0.001 :: Double
  let ns = [10, 20 .. 120]
  putStrLn "N, time, qubits"
  forM_ ns $ \n -> do
    let prog = sprog & P.mapSize (Sym.unSym . Sym.subst "N" (Sym.con n))
    adj <- replicateM n (replicateM n (randomIO :: IO Bool))
    let f = \case
          [P.FinV u, P.FinV v] -> [P.toValue $ adj !! u !! v]
          _ -> undefined
    ExptResult{wallTime, numQubits} <- runExpt' prog eps (Ctx.singleton "Adj" f)
    putStrLn $ printf "%d, %.5f, %d" n wallTime numQubits
main :: IO ()
main = do
  putStrLn "Experiment: Wall-time vs Qubit Count\n"
  -- matrixSearchExpt
  -- hillClimbExpt
  -- depth3NAND
  triangleFinding
  putStrLn "end"

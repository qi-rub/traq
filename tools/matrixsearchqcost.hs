module Main (main) where

import Control.Monad
import System.IO
import System.Random
import System.Random.Shuffle (shuffleM)
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import qualified QCompose.ProtoLang as P

import QCompose.Examples.MatrixSearch

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

matToBinFun :: [[Value]] -> ([Value] -> [Value])
matToBinFun mat [i, j] = [mat !! fromIntegral i !! fromIntegral j]
matToBinFun _ _ = error "unsupported"

-- | Get the input-dependent quantum query cost.
qcost ::
  -- | eps (max. fail probability)
  Double ->
  -- | matrix
  [[Value]] ->
  Double
qcost eps mat = cost
 where
  n = length mat
  m = length $ head mat

  ex = matrixExampleS n m

  dataCtx = Ctx.singleton "Oracle" (matToBinFun mat)

  cost = P.quantumQueryCostBound eps ex "Oracle" dataCtx Ctx.empty

randomMatrix :: SizeT -> SizeT -> IO [[Value]]
randomMatrix n m = do
  replicateM n $ replicateM m $ randomRIO (0, 1)

randomMatrixWith ::
  -- (n, m)
  (Int, Int) ->
  -- | number of good rows
  Int ->
  -- | number of zeros in each non-solution row
  Int ->
  IO [[Value]]
randomMatrixWith (n, m) g z = do
  guard $ 1 <= n && 1 <= m
  guard $ 0 <= g && g <= n
  guard $ 1 <= z && z <= m

  bad_rows <- replicateM (n - g) $ do
    shuffleM $ replicate z 0 ++ replicate (m - z) 1

  let rows = bad_rows ++ replicate g (replicate m 1)
  shuffleM rows

randomStat :: SizeT -> Double -> Int -> IO [Double]
randomStat nruns eps n =
  replicateM nruns $ do
    mat <- randomMatrix n n
    return $ qcost eps mat

main :: IO ()
main = do
  putStrLn "Matrix Search: Quantum Costs"
  printDivider

  -- forM_ [0 .. 50] $ \t -> do
  --   let n = 50 :: Int
  --   let eps = 0.001 :: Double
  --   putStrLn $ printf "t=%d: %f" t (_EQSearch n t eps)

  withFile "examples/matrix_search/stats/qcost.csv" WriteMode $ \h -> do
    hPutStrLn h "eps,n,cost"
    forM_ [0.001, 0.0005, 0.0001] $ \eps -> do
      forM_ [10, 20 .. 100] $ \n -> do
        cs <- randomStat 20 eps n
        forM cs $ \c -> do
          hPutStrLn h $ printf "%f,%d,%.2f" eps n c

  withFile "examples/matrix_search/stats/datadep.csv" WriteMode $ \h -> do
    hPutStrLn h "eps,n,m,good,zeros,cost"
    let eps = 0.001
    forM_ [10, 20 .. 100] $ \n -> do
      let m = n
      forM_ [0 .. n `div` 4 + 1] $ \g -> do
        forM_ [1] $ \z -> do
          mat <- randomMatrixWith (n, m) g z
          let c = qcost eps mat
          hPutStrLn h $ printf "%f,%d,%d,%d,%d,%.2f" eps n m g z c

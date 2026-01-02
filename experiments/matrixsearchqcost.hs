{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad
import System.IO
import System.Random
import System.Random.Shuffle (shuffleM)
import System.TimeIt (timeIt)
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Examples.MatrixSearch
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.DetSearch (DetSearch (..))
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.RandomSearch (RandomSearch (..))
import qualified Traq.ProtoLang as P
import qualified Traq.Analysis as P

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

-- | Data to box a type @p@
data Phantom p = Phantom

class
  ( P.QuantumExpCost p SizeT Double
  , SizeType p ~ SizeT
  , PrecType p ~ Double
  ) =>
  MyPrim p
  where
  mkAny :: P.VarType SizeT -> PartialFun -> p

  matrixExample :: SizeT -> SizeT -> P.Program p
  matrixExample = mkMatrixExample (\t f -> P.PrimCallE $ mkAny t f)

instance MyPrim (DefaultPrims SizeT Double) where
  mkAny ty fn = QAny $ Primitive [fn] $ QSearchCFNW $ PrimSearch AnyK ty

instance MyPrim (Primitive (RandomSearch SizeT Double)) where
  mkAny ty fn = Primitive [fn] $ RandomSearch $ PrimSearch AnyK ty

instance MyPrim (Primitive (QSearchCFNW SizeT Double)) where
  mkAny ty fn = Primitive [fn] $ QSearchCFNW $ PrimSearch AnyK ty

instance MyPrim (Primitive (DetSearch SizeT Double)) where
  mkAny ty fn = Primitive [fn] $ DetSearch $ PrimSearch AnyK ty

defPrims :: Phantom (DefaultPrims SizeT Double)
defPrims = Phantom

randSearchP :: Phantom (Primitive (RandomSearch SizeT Double))
randSearchP = Phantom

qSearchP :: Phantom (Primitive (QSearchCFNW SizeT Double))
qSearchP = Phantom

detSearchP :: Phantom (Primitive (DetSearch SizeT Double))
detSearchP = Phantom

type Value = P.Value SizeT

class MatrixType t where
  nRows, nCols :: t -> Int

  toValueFun :: t -> [Value] -> [Value]

instance MatrixType (SizeT, SizeT, Value -> Value -> Bool) where
  nRows (n, _, _) = fromIntegral n

  nCols (_, m, _) = fromIntegral m

  toValueFun (_, _, mat) [i, j] = [P.toValue $ mat i j]
  toValueFun _ _ = error "unsupported"

instance MatrixType [[Value]] where
  nRows = length
  nCols = length . head

  toValueFun mat [P.FinV i, P.FinV j] = [mat !! fromIntegral i !! fromIntegral j]
  toValueFun _ _ = error "unsupported"

-- | Get the input-dependent quantum query cost.
qcost ::
  forall primsT matT.
  (MatrixType matT, MyPrim primsT) =>
  Phantom primsT ->
  -- | eps (max. fail probability)
  P.FailProb Double ->
  -- | matrix
  matT ->
  Double
qcost _ eps mat = getCost cost
 where
  n = nRows mat
  m = nCols mat

  ex = matrixExample @primsT n m

  dataCtx = Ctx.singleton "Matrix" (toValueFun mat)

  cost = P.quantumQueryCost @_ @Double @(SimpleQueryCost Double) P.SplitUsingNeedsEps eps ex dataCtx []

randomMatrix :: SizeT -> SizeT -> IO [[Value]]
randomMatrix n m = do
  replicateM n $ replicateM m $ P.FinV <$> randomRIO (0, 1)

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
    shuffleM $ replicate z (P.FinV 0) ++ replicate (m - z) (P.FinV 1)

  let rows = bad_rows ++ replicate g (replicate m (P.FinV 1))
  shuffleM rows

randomStat :: (MyPrim primsT) => Phantom primsT -> SizeT -> P.FailProb Double -> Int -> IO [Double]
randomStat phantom nruns eps n =
  replicateM nruns $ do
    mat <- randomMatrix n n
    return $ qcost phantom eps mat

computeStatsForRandomMatrices :: (MyPrim primsT) => Phantom primsT -> IO ()
computeStatsForRandomMatrices phantom =
  withFile "examples/matrix_search/stats/qcost.csv" WriteMode $ \h -> do
    hPutStrLn h "eps,n,cost"
    forM_ [0.001, 0.0005, 0.0001] $ \eps -> do
      forM_ [10, 20 .. 100] $ \n -> do
        cs <- randomStat phantom 20 (P.failProb eps) n
        forM cs $ \c -> do
          hPutStrLn h $ printf "%f,%d,%.2f" eps n c

computeStatsForPlantedRandomMatrices :: (MyPrim primsT) => Phantom primsT -> IO ()
computeStatsForPlantedRandomMatrices phantom =
  withFile "examples/matrix_search/stats/datadep.csv" WriteMode $ \h -> do
    hPutStrLn h "eps,n,m,good,zeros,cost"
    let eps = 0.001
    forM_ [10, 20 .. 100] $ \n -> do
      let m = n
      forM_ [0 .. n `div` 4 + 1] $ \g -> do
        forM_ [1] $ \z -> do
          mat <- randomMatrixWith (n, m) g z
          let c = qcost phantom (P.failProb eps) mat
          hPutStrLn h $ printf "%f,%d,%d,%d,%d,%.2f" eps n m g z c

computeStatsForWorstCaseMatrices :: (MyPrim primsT) => Phantom primsT -> IO ()
computeStatsForWorstCaseMatrices phantom =
  withFile "examples/matrix_search/stats/worstcase.csv" WriteMode $ \h -> do
    hPutStrLn h "n,cost"
    let eps = 0.001
    -- forM_ ((10 :: Int) : [500, 1000 .. 4000]) $ \n -> do
    forM_ ((10 :: Int) : [20, 40, 100]) $ \n -> do
      putStrLn $ ">> n = " <> show n
      let m = n
      let c = qcost phantom (P.failProb eps) (n, m, matfun)
      hPutStrLn h $ printf "%d,%.2f" n c
 where
  matfun :: Value -> Value -> Bool
  matfun i _ = i == P.FinV 0

main :: IO ()
main = do
  putStrLn "Matrix Search: Quantum Costs"
  printDivider

  -- forM_ [0 .. 50] $ \t -> do
  --   let n = 50 :: Int
  --   let eps = 0.001 :: Double
  --   putStrLn $ printf "t=%d: %f" t (_EQSearch n t eps)

  -- computeStatsForRandomMatrices
  -- computeStatsForPlantedRandomMatrices
  timeIt $ computeStatsForWorstCaseMatrices detSearchP
  timeIt $ computeStatsForWorstCaseMatrices qSearchP
  -- timeIt computeStatsForWorstCaseExample
  -- timeIt triangular
  putStrLn "done"

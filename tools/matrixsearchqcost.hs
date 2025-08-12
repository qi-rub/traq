{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad
import Lens.Micro.GHC
import System.IO
import System.Random
import System.Random.Shuffle (shuffleM)
import System.TimeIt (timeIt)
import Text.Parsec.String (parseFromFile)
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Traq.Examples.MatrixSearch
import Traq.Primitives.Search.DetSearch (DetSearch)
import Traq.Primitives.Search.Prelude (HasPrimAny)
import Traq.Primitives.Search.QSearchCFNW (QSearchCFNW)
import Traq.Primitives.Search.RandomSearch (RandomSearch)

printDivider :: IO ()
printDivider = putStrLn $ replicate 80 '='

-- | Data to box a type @p@
data Phantom p = Phantom

class
  ( P.CanParsePrimitive p
  , P.QuantumCostablePrimitive p p SizeT Double
  , HasPrimAny p
  ) =>
  MyPrim p

instance MyPrim DefaultPrims
instance MyPrim RandomSearch
instance MyPrim QSearchCFNW
instance MyPrim DetSearch

defPrims :: Phantom DefaultPrims
defPrims = Phantom

randSearchP :: Phantom RandomSearch
randSearchP = Phantom

qSearchP :: Phantom QSearchCFNW
qSearchP = Phantom

detSearchP :: Phantom DetSearch
detSearchP = Phantom

type Value = P.Value SizeT

class MatrixType t where
  nRows, nCols :: t -> Int

  toValueFun :: t -> [Value] -> [Value]

instance MatrixType (Value, Value, Value -> Value -> Bool) where
  nRows (P.FinV n, _, _) = fromIntegral n
  nCols (_, P.FinV m, _) = fromIntegral m

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
  Double ->
  -- | matrix
  matT ->
  Double
qcost _ eps mat = cost
 where
  n = nRows mat
  m = nCols mat

  ex = matrixExample @primsT n m P.tbool

  dataCtx = Ctx.singleton "Oracle" (toValueFun mat)
  ticks = mempty & at "Oracle" ?~ 1.0

  cost = P.quantumQueryCost P.SplitUsingNeedsEps eps ex ticks ticks dataCtx Ctx.empty

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

randomStat :: (MyPrim primsT) => Phantom primsT -> SizeT -> Double -> Int -> IO [Double]
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
        cs <- randomStat phantom 20 eps n
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
          let c = qcost phantom eps mat
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
      let c = qcost phantom eps (P.FinV n, P.FinV m, matfun)
      hPutStrLn h $ printf "%d,%.2f" n c
 where
  matfun :: Value -> Value -> Bool
  matfun i _ = i == P.FinV 0

computeStatsForWorstCaseExample :: IO ()
computeStatsForWorstCaseExample = do
  Right sprog <- parseFromFile (P.programParser @DefaultPrims) "examples/matrix_search/worstcase.qb"
  let getprog n = fmap (Sym.unSym . Sym.subst "M" (Sym.con n) . Sym.subst "N" (Sym.con n)) sprog

  withFile "examples/matrix_search/stats/worstcase.csv" WriteMode $ \h -> do
    hPutStrLn h "n,cost"

    let eps = 0.5 :: Double
    forM_ (10 : [500, 1000 .. 10000]) $ \n -> do
      let ex = getprog n
      let ticks = mempty & at "Oracle" ?~ 1.0
      let c = P.quantumQueryCost P.SplitSimple eps ex ticks ticks Ctx.empty Ctx.empty
      hPutStrLn h $ printf "%d,%.2f" n c

triangular :: IO ()
triangular = do
  Right sprog <- parseFromFile (P.programParser @DefaultPrims) "examples/matrix_search/triangular.qb"
  let getprog n = fmap (Sym.unSym . Sym.subst "M" (Sym.con n) . Sym.subst "N" (Sym.con n)) sprog

  withFile "examples/matrix_search/stats/triangular.csv" WriteMode $ \h -> do
    hPutStrLn h "n,cost"

    let eps = 0.2 :: Double
    -- forM_ (10 : [500, 1000 .. 5000]) $ \n -> do
    forM_ [5500, 6000] $ \n -> do
      putStrLn $ printf "running n: %d" n
      let ex = getprog n
      let ticks = mempty & at "Oracle" ?~ 1.0
      let c = P.quantumQueryCost P.SplitSimple eps ex ticks ticks Ctx.empty Ctx.empty
      hPutStrLn h $ printf "%d,%.2f" n c
      putStrLn $ printf "cost: %.2f, ratio: %f" c (c / fromIntegral (n ^ (2 :: Int)))

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

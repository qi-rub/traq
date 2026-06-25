{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Generate the augmented case-study statistics table (Section 8.2 / Table 2).

For every case study we report, at its paper/Makefile default parameters:

  * LoC of the source @.traq@ program,
  * size of the compiled QPL program (LoC of the @.qpl@ and qubit count),
  * the proven error bound (worst-case total-variation failure probability), and
  * the proven cost bound (worst-case quantum query cost).

The error/cost bounds are produced in two forms:

  * concrete   - a single number, after distributing the @eps@ budget across the
                 primitive calls ('A.annotateProgWithErrorBudget'), exactly like
                 the QPL backend of the @traq@ CLI;
  * symbolic   - an expression in the per-call error budgets @eps_i@ (sizes are
                 concrete; budget is left undistributed via 'A.annSymEpsProg'),
                 exactly like the @-t Symbolic@ backend of the @traq@ CLI.

This mirrors @tools/traq.hs@ (loadTraqProgram / emitQPL / emitSymbolic) and
@experiments/compile_loc.hs@; we just fix the primitive type to
'P.WorstCasePrims' (the type the CLI parses every example with) and tabulate.
-}
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Data.List (intercalate)
import System.IO (IOMode (WriteMode), hPutStrLn, stderr, withFile)
import Text.Parsec.String (parseFromFile)
import Text.Printf (printf)

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- Case-study configuration
-- ============================================================

data CaseStudy = CaseStudy
  { csLabel :: String
  , csDomain :: String
  , csFile :: FilePath
  , csPrims :: String
  , csSizes :: [(Ident, SizeT)]
  , csFloats :: [(Ident, Double)]
  , csEps :: Double
  }

-- | The 9 case-study programs (Max-k-SAT contributes two), with the same
-- parameters their Makefiles use to build the committed @.qpl@ files.
caseStudies :: [CaseStudy]
caseStudies =
  [ CaseStudy
      { csLabel = "Triangle Finding"
      , csDomain = "Search"
      , csFile = "examples/search/triangle_finding.traq"
      , csPrims = "search"
      , csSizes = [("N", 10)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Farthest Points"
      , csDomain = "Search"
      , csFile = "examples/search/clustering.traq"
      , csPrims = "search"
      , csSizes = [("N", 10), ("M", 100)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Matrix Search"
      , csDomain = "Search"
      , csFile = "examples/matrix_search/matrix_search.traq"
      , csPrims = "search, all"
      , csSizes = [("N", 20), ("M", 10)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Depth-3 NAND"
      , csDomain = "Search"
      , csFile = "examples/matrix_search/depth3_NAND_formula.traq"
      , csPrims = "all"
      , csSizes = [("N", 20), ("M", 10), ("K", 10)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Max-k-SAT (simple)"
      , csDomain = "Optimization"
      , csFile = "examples/hillclimb/max_sat_hillclimb.traq"
      , csPrims = "search"
      , csSizes = [("n", 20), ("W", 1000)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Max-k-SAT (steep)"
      , csDomain = "Optimization"
      , csFile = "examples/hillclimb/steep_max_sat.traq"
      , csPrims = "argmax"
      , csSizes = [("n", 20), ("W", 1000)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "0/1 Knapsack"
      , csDomain = "Optimization"
      , csFile = "examples/tree_generator/tree_generator_01_knapsack.traq"
      , csPrims = "amplify"
      , csSizes = [("W", 1000), ("P", 1000), ("N", 4), ("K", 3)]
      , csFloats = [("p", 0.2)]
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "3-Round Feistel"
      , csDomain = "Cryptanalysis"
      , csFile = "examples/cryptanalysis/3_round_feistel.traq"
      , csPrims = "simon"
      , csSizes = [("n", 20), ("n_plus_1", 21)]
      , csFloats = []
      , csEps = 1e-3
      }
  , CaseStudy
      { csLabel = "Even-Mansour"
      , csDomain = "Cryptanalysis"
      , csFile = "examples/cryptanalysis/even_mansour.traq"
      , csPrims = "simon"
      , csSizes = [("n", 20), ("n_plus_1", 21)]
      , csFloats = []
      , csEps = 1e-3
      }
  ]

-- ============================================================
-- Parameter substitution (mirrors tools/traq.hs)
-- ============================================================

subsOnce :: (Num a, Eq a) => (Ident, a) -> Sym.Sym a -> Sym.Sym a
subsOnce (k, v) = Sym.subst k (Sym.con v)

-- | Substitute concrete sizes, collapsing the symbolic size to a concrete 'SizeT'.
subsSizes :: [(Ident, SizeT)] -> Sym.Sym SizeT -> SizeT
subsSizes ps s = Sym.unSym $ foldr subsOnce s ps

-- | Substitute concrete float parameters into the (symbolic) precision.
subsFloats :: [(Ident, Double)] -> Sym.Sym Double -> Double
subsFloats ps s = Sym.unSym $ foldr subsOnce s ps

-- | Substitute size parameters that appear inside *precision* expressions,
-- keeping the result symbolic (so float params / @eps_i@ stay free). Needed
-- because some programs reference a size in a precision position — e.g. the
-- knapsack's @amplify<p ** N>@ — which the plain size substitution misses.
subsSizesInPrec :: [(Ident, SizeT)] -> Sym.Sym Double -> Sym.Sym Double
subsSizesInPrec ps s = foldr (\(k, v) -> Sym.subst k (Sym.con (fromIntegral v))) s ps

-- | Size parameters as float substitutions, for the concrete precision pass.
sizesAsFloats :: [(Ident, SizeT)] -> [(Ident, Double)]
sizesAsFloats = map (fmap fromIntegral)

type ParsedPrim = P.WorstCasePrims (Sym.Sym SizeT) (Sym.Sym Double)
type SymPrim = P.WorstCasePrims SizeT (Sym.Sym Double)
type ConcPrim = P.WorstCasePrims SizeT Double

loadProgram :: FilePath -> IO (CPL.Program ParsedPrim)
loadProgram fname = do
  res <- parseFromFile (CPL.programParser @ParsedPrim) fname
  either (fail . show) pure res

-- ============================================================
-- Concrete and symbolic analysis passes
-- ============================================================

-- | Concrete metrics at the given sizes/floats with the budget @eps@ distributed
-- over the primitive calls: (QPL LoC, qubits, error bound, cost bound).
concretePass :: CaseStudy -> IO (Either String (Int, Int, Double, Double))
concretePass CaseStudy{..} = handleAny $ do
  parsed <- loadProgram csFile
  let progSym = CPL.mapSize (subsSizes csSizes) parsed :: CPL.Program SymPrim
      progConc = CPL.mapPrec (subsFloats (csFloats ++ sizesAsFloats csSizes)) progSym :: CPL.Program ConcPrim
  progAnn <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb csEps) progConc
  qpl <- either fail pure $ Compiler.lowerProgram progAnn
  let loc = length . lines $ PP.toCodeString qpl
      qubits = QPL.numQubits qpl
      cost = getCost (A.costQProg progAnn :: SimpleQueryCost Double)
      err = A.getFailProb (A.tvErrorQProg progAnn) :: Double
  loc' <- evaluate loc
  qubits' <- evaluate qubits
  err' <- evaluate err
  cost' <- evaluate cost
  pure (loc', qubits', err', cost')

-- | Symbolic metrics at the given sizes, with per-call budgets @eps_i@ left free:
-- (error bound expression, cost bound expression).
symbolicPass :: CaseStudy -> IO (Either String (String, String))
symbolicPass CaseStudy{..} = handleAny $ do
  parsed <- loadProgram csFile
  let progSym =
        CPL.mapPrec (subsSizesInPrec csSizes) $
          CPL.mapSize (subsSizes csSizes) parsed ::
          CPL.Program SymPrim
  progAnn <- either fail pure $ A.annSymEpsProg progSym
  let err = Sym.simpl $ A.getFailProb (A.tvErrorQProg progAnn)
      cost = Sym.simpl $ getCost (A.costQProg progAnn :: SimpleQueryCost (Sym.Sym Double))
      errS = show err
      costS = show cost
  errS' <- evaluate (length errS `seq` errS)
  costS' <- evaluate (length costS `seq` costS)
  pure (errS', costS')

handleAny :: forall a. IO a -> IO (Either String a)
handleAny act = do
  res <- try act :: IO (Either SomeException a)
  pure $ either (Left . show) Right res

-- ============================================================
-- A fully-computed row
-- ============================================================

data Row = Row
  { rStudy :: CaseStudy
  , rTraqLoC :: Int
  , rConc :: Either String (Int, Int, Double, Double)
  , rSym :: Either String (String, String)
  }

computeRow :: CaseStudy -> IO Row
computeRow cs = do
  traqLoC <- length . lines <$> readFile (csFile cs)
  conc <- concretePass cs
  sym <- symbolicPass cs
  case conc of
    Left e -> hPutStrLn stderr $ "[" ++ csLabel cs ++ "] concrete pass failed: " ++ e
    Right _ -> pure ()
  case sym of
    Left e -> hPutStrLn stderr $ "[" ++ csLabel cs ++ "] symbolic pass failed: " ++ e
    Right _ -> pure ()
  pure $ Row cs traqLoC conc sym

-- ============================================================
-- Formatting
-- ============================================================

-- | Render a cost number compactly: integral values without a fraction.
num :: Double -> String
num x
  | x == fromInteger (round x) = show (round x :: Integer)
  | otherwise = printf "%.2f" x

-- | Render a (small) failure probability with scientific precision.
numProb :: Double -> String
numProb x
  | x == 0 = "0"
  | otherwise = printf "%.2e" x

mdTable :: [Row] -> String
mdTable rows = unlines (header : sep : map rowLine rows)
 where
  header =
    cells
      ["Program", "Domain", "LoC", "Primitives", "|QPL| (LoC)", "qubits", "error bound (≤)", "cost bound"]
  sep = cells (replicate 8 "---")
  rowLine Row{..} =
    let CaseStudy{..} = rStudy
        (qloc, qub, err, cost) = case rConc of
          Right (a, b, c, d) -> (show a, show b, numProb c, num d)
          Left _ -> ("ERR", "ERR", "ERR", "ERR")
     in cells [csLabel, csDomain, show rTraqLoC, csPrims, qloc, qub, err, cost]
  cells xs = "| " ++ intercalate " | " xs ++ " |"

symbolicAppendix :: [Row] -> String
symbolicAppendix rows = unlines $ concatMap block rows
 where
  block Row{..} =
    let CaseStudy{..} = rStudy
        (errS, costS) = case rSym of
          Right (e, c) -> (e, c)
          Left e -> ("(unavailable: " ++ e ++ ")", "(unavailable)")
     in [ "### " ++ csLabel
        , "error bound (symbolic):  " ++ errS
        , "cost bound  (symbolic):  " ++ costS
        , ""
        ]

csvQuote :: String -> String
csvQuote s
  | any (`elem` ",\"\n") s = '"' : concatMap esc s ++ "\""
  | otherwise = s
 where
  esc '"' = "\"\""
  esc c = [c]

writeCsv :: FilePath -> [Row] -> IO ()
writeCsv path rows = withFile path WriteMode $ \h -> do
  hPutStrLn h $
    intercalate
      ","
      [ "program"
      , "domain"
      , "traq_loc"
      , "primitives"
      , "qpl_loc"
      , "qubits"
      , "eps_budget"
      , "error_concrete"
      , "cost_concrete"
      , "error_symbolic"
      , "cost_symbolic"
      ]
  mapM_ (hPutStrLn h . csvRow) rows
 where
  csvRow Row{..} =
    let CaseStudy{..} = rStudy
        (qloc, qub, err, cost) = case rConc of
          Right (a, b, c, d) -> (show a, show b, numProb c, num d)
          Left e -> let m = "ERR:" ++ e in (m, m, m, m)
        (errS, costS) = case rSym of
          Right (e, c) -> (e, c)
          Left e -> ("ERR:" ++ e, "ERR:" ++ e)
     in intercalate "," $
          map
            csvQuote
            [ csLabel
            , csDomain
            , show rTraqLoC
            , csPrims
            , qloc
            , qub
            , printf "%g" csEps
            , err
            , cost
            , errS
            , costS
            ]

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  rows <- forM caseStudies computeRow

  putStrLn "## Case-study statistics (Table 2, augmented)\n"
  putStrLn "Concrete bounds are at each program's default parameters (Makefile sizes,"
  putStrLn "failure-probability budget ε = 10⁻³). `error bound` is the proven worst-case"
  putStrLn "total-variation failure probability (≤ ε); `cost bound` is the proven worst-case"
  putStrLn "quantum query cost (costQ).\n"
  putStr $ mdTable rows

  putStrLn "\n## Symbolic bounds\n"
  putStrLn "Expressions at the same sizes, with the per-primitive error budgets `eps_i`"
  putStrLn "left undistributed (and any float parameters symbolic). These are exactly what"
  putStrLn "`traq -t Symbolic` emits.\n"
  putStr $ symbolicAppendix rows

  writeCsv "examples/case_study_stats.csv" rows
  putStrLn "Wrote examples/case_study_stats.csv"

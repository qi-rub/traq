{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.List (isPrefixOf)
import Options.Applicative
import System.FilePath (takeExtension)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as Analysis
import Traq.Analysis.CostModel.QueryCost (QueryCost)
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Experimental.Compiler.Qiskit as Qiskit
import qualified Traq.Experimental.Compiler.Qualtran as Qualtran
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- CLI
-- ============================================================

data Backend = QPL | Qualtran | Qiskit | Symbolic
  deriving (Read, Show, Eq)

data Options = Options
  { target :: Backend
  , in_file :: FilePath
  , out_file :: Maybe FilePath
  , eps :: Maybe Double
  , params :: [(Ident, SizeT)]
  , paramsf :: [(Ident, Double)]
  , experimental :: Bool
  }
  deriving (Show)

-- ============================================================
-- Core
-- ============================================================

-- | Load a Traq program source, and substitute parameters.
loadTraqProgram :: ReaderT Options IO (CPL.Program (P.WorstCasePrims SizeT Double))
loadTraqProgram = do
  prog <- loadTraqProgramSym
  pf <- view (to paramsf)
  pure $ CPL.mapPrec (subs_paramsf pf) prog

-- | Load a Traq program source, substituting only size parameters. Precision
-- stays as 'Sym.Sym Double' so epsilons remain symbolic for the Symbolic backend.
loadTraqProgramSym :: ReaderT Options IO (CPL.Program (P.WorstCasePrims SizeT (Sym.Sym Double)))
loadTraqProgramSym = do
  code <- lift . readFile =<< view (to in_file)
  case CPL.parseProgram @(P.WorstCasePrims _ (Sym.Sym Double)) code of
    Left err -> fail $ show err
    Right prog -> do
      ps <- view (to params)
      pure $ CPL.mapSize (subs_params ps) prog

subsOnce :: (Num a, Eq a) => (Ident, a) -> Sym.Sym a -> Sym.Sym a
subsOnce (k, v) = Sym.subst k (Sym.con v)

subs_params :: [(Ident, SizeT)] -> (Sym.Sym Int -> SizeT)
subs_params params s = Sym.unSym $ foldr subsOnce s params

subs_paramsf :: [(Ident, Double)] -> (Sym.Sym Double -> Double)
subs_paramsf pf s = Sym.unSym $ foldr subsOnce s pf

-- | Compile source CPL to target QPL.
compileCPL :: (RealFloat prec, Show prec) => CPL.Program (P.WorstCasePrims SizeT prec) -> prec -> IO (QPL.Program SizeT)
compileCPL prog eps = do
  prog' <- either fail pure $ Analysis.annotateProgWithErrorBudget (Analysis.failProb eps) prog
  either fail pure $ Compiler.lowerProgram prog'

-- | Load a serialized QPL Program AST.
loadQPLProgram :: ReaderT Options IO (QPL.Program SizeT)
loadQPLProgram = do
  raw <- lift . readFile =<< view (to in_file)
  return $ read raw

-- ============================================================
-- Backends
-- ============================================================

emitQPL :: QPL.Program SizeT -> IO String
emitQPL qpl_prog = do
  let nqubits = QPL.numQubits qpl_prog
  pure $ unlines [PP.toCodeString qpl_prog, printf "// qubits: %d" nqubits]

-- | Insert generated code at the ===CODE-HERE=== marker in a template file.
emitWithTemplate :: FilePath -> String -> IO String
emitWithTemplate templatePath code = do
  template <- readFile templatePath
  let marker = "# ===CODE-HERE==="
  pure $ case break (marker `isPrefixOf`) (lines template) of
    (before, _ : after) -> unlines $ before ++ [code] ++ after
    _ -> unlines [template, code]

emitQualtran :: QPL.Program SizeT -> IO String
emitQualtran = emitWithTemplate "tools/qualtran_prelude.py" . Qualtran.toPy

emitQiskit :: QPL.Program SizeT -> IO String
emitQiskit = emitWithTemplate "tools/qiskit_prelude.py" . Qiskit.toPy

{- | Attach a fresh symbolic eps_i to each primitive in the (size-substituted,
symbolic-precision) program, then dump:
    * the total-error constraint from 'tvErrorQProg' (used as ... <= budget),
    * 'costU', 'costQ' as QueryCost expressions in the eps_i.
'expCostQ' is omitted because it requires external-function interpretations
that are program-specific and not available from a generic CLI flag.
-}
emitSymbolic :: CPL.Program (P.WorstCasePrims SizeT (Sym.Sym Double)) -> IO String
emitSymbolic prog = do
  prog_ann <- either fail pure $ Analysis.annSymEpsProg prog
  let tvErr = Sym.simpl $ Analysis.getFailProb $ Analysis.tvErrorQProg prog_ann
  let costU_ = fmap Sym.simpl (Analysis.costUProg prog_ann :: QueryCost (Sym.Sym Double))
  let costQ_ = fmap Sym.simpl (Analysis.costQProg prog_ann :: QueryCost (Sym.Sym Double))
  pure $
    unlines
      [ "# Symbolic epsilon extraction"
      , ""
      , "## Error-budget constraint (tvErrorQ)"
      , "# Solver constraint: the following expression <= user-chosen budget."
      , show tvErr
      , ""
      , "## costU (worst-case unitary cost, per external fn)"
      , show costU_
      , ""
      , "## costQ (worst-case quantum cost, per external fn)"
      , show costQ_
      ]

-- ============================================================
-- CLI parser
-- ============================================================
parseKeyValue :: (Read a) => String -> Maybe (String, a)
parseKeyValue s = do
  let (key, rest) = break (== '=') s
  valS <- case rest of
    '=' : v -> Just v
    _ -> Nothing
  val <- readMaybe valS
  return (key, val)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> header "Traq: Compile CPL programs to QPL, Qualtran, or Qiskit.")
 where
  options = do
    target <-
      option auto $
        long "target"
          <> short 't'
          <> metavar "TARGET"
          <> help "Output target: QPL | Qualtran (experimental) | Qiskit (experimental)"
          <> value QPL
          <> showDefault

    in_file <-
      strOption $
        long "input"
          <> short 'i'
          <> metavar "INPUT"
          <> help "Input file (.traq or .qpl)"

    out_file <-
      optional $
        strOption $
          long "output"
            <> short 'o'
            <> metavar "OUTPUT"
            <> help "Output file (default: stdout)"

    eps <-
      optional $
        option auto $
          long "failprob"
            <> short 'p'
            <> metavar "FLOAT"
            <> help "The maximum failure probability of the entire program"

    params <-
      many $
        option (maybeReader parseKeyValue) $
          long "arg"
            <> help "parameters..."
            <> metavar "NAME=VALUE"

    paramsf <-
      many $
        option (maybeReader parseKeyValue) $
          long "argf"
            <> help "float parameters..."
            <> metavar "NAME=VALUE"

    experimental <-
      switch $
        long "experimental"
          <> help "Enable experimental features"

    pure Options{..}

main :: IO ()
main = do
  options@Options{..} <- execParser opts

  let guardExperimental feat =
        unless experimental $
          fail $
            "feature " <> feat <> " is experimental; pass --experimental to run it anyway"

  out_str <- case target of
    Symbolic -> do
      unless (takeExtension in_file == ".traq") $
        fail "Symbolic backend requires a .traq input"
      p <- runReaderT loadTraqProgramSym options
      emitSymbolic p
    _ -> do
      qpl_prog <- (runReaderT ?? options) $ do
        case takeExtension in_file of
          ".traq" -> do
            p <- loadTraqProgram
            case eps of
              Just e -> lift $ compileCPL p e
              Nothing -> fail "--failprob is required when compiling .traq files"
          ".qpl" -> loadQPLProgram
          ext -> fail $ "Unsupported file extension: " ++ ext
      case target of
        QPL -> emitQPL qpl_prog
        Qualtran -> guardExperimental "backend:Qualtran" >> emitQualtran qpl_prog
        Qiskit -> guardExperimental "backend:Qiskit" >> emitQiskit qpl_prog
  maybe putStr writeFile out_file out_str

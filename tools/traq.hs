{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Options.Applicative
import System.FilePath (takeExtension)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as Analysis
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Qiskit as Qiskit
import qualified Traq.Compiler.Qualtran as Qualtran
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- CLI
-- ============================================================

data Backend = QPL | Qualtran | Qiskit
  deriving (Read, Show, Eq)

data Options = Options
  { target :: Backend
  , in_file :: FilePath
  , out_file :: Maybe FilePath
  , eps :: Maybe Double
  , params :: [(Ident, SizeT)]
  , paramsf :: [(Ident, Double)]
  }
  deriving (Show)

-- ============================================================
-- Core
-- ============================================================

-- | Load a Traq program source, and substitute parameters.
loadTraqProgram :: ReaderT Options IO (CPL.Program (P.WorstCasePrims SizeT Double))
loadTraqProgram = do
  code <- lift . readFile =<< view (to in_file)
  case CPL.parseProgram @(P.WorstCasePrims _ (Sym.Sym Double)) code of
    Left err -> fail $ show err
    Right prog -> do
      ps <- view (to params)
      pf <- view (to paramsf)
      pure $ CPL.mapPrec (subs_paramsf ps pf) $ CPL.mapSize (subs_params ps) prog
 where
  subsOnce :: (Num a, Eq a) => (Ident, a) -> Sym.Sym a -> Sym.Sym a
  subsOnce (k, v) = Sym.subst k (Sym.con v)

  subs_params :: [(Ident, SizeT)] -> (Sym.Sym Int -> SizeT)
  subs_params params s = Sym.unSym $ foldr subsOnce s params

  subs_paramsf :: [(Ident, SizeT)] -> [(Ident, Double)] -> (Sym.Sym Double -> Double)
  subs_paramsf ps pf s = Sym.unSym $ foldr subsOnce (foldr subsI s ps) pf
   where
    subsI :: (Ident, SizeT) -> Sym.Sym Double -> Sym.Sym Double
    subsI (k, v) = subsOnce (k, fromIntegral v)

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

emitQualtran :: QPL.Program SizeT -> IO String
emitQualtran qpl_prog = do
  py_preamble <- readFile "tools/qualtran_prelude.py"
  let py_prog_str = Qualtran.toPy qpl_prog
  pure $ unlines [py_preamble, py_prog_str]

emitQiskit :: QPL.Program SizeT -> IO String
emitQiskit qpl_prog = do
  py_preamble <- readFile "tools/qiskit_prelude.py"
  let py_prog_str = Qiskit.toPy qpl_prog
  pure $ unlines [py_preamble, py_prog_str]

-- ============================================================
-- CLI parser
-- ============================================================

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> header "Traq: Compile CPL programs to QPL, Qualtran, or Qiskit.")
 where
  options =
    Options
      <$> option
        auto
        ( long "target"
            <> short 't'
            <> metavar "TARGET"
            <> help "Output target: QPL | Qualtran | Qiskit"
            <> value QPL
            <> showDefault
        )
      <*> strOption
        ( long "input"
            <> short 'i'
            <> metavar "INPUT"
            <> help "Input file (.traq or .qpl)"
        )
      <*> optional
        ( strOption
            ( long "output"
                <> short 'o'
                <> metavar "OUTPUT"
                <> help "Output file (default: stdout)"
            )
        )
      <*> optional
        ( option
            auto
            ( long "failprob"
                <> short 'p'
                <> metavar "FLOAT"
                <> help "The maximum failure probability of the entire program"
            )
        )
      <*> many (option (maybeReader parseKeyValue) (long "arg" <> help "parameters..." <> metavar "NAME=VALUE"))
      <*> many (option (maybeReader parseKeyValueF) (long "argf" <> help "float parameters..." <> metavar "NAME=VALUE"))

  parseKeyValue s = do
    let key = takeWhile (/= '=') s
    let valS = tail $ dropWhile (/= '=') s
    val <- readMaybe valS
    return (key, val)

  parseKeyValueF s = do
    let key = takeWhile (/= '=') s
    let valS = tail $ dropWhile (/= '=') s
    val <- readMaybe valS
    return (key, val :: Double)

main :: IO ()
main = do
  options@Options{..} <- execParser opts

  qpl_prog <- (runReaderT ?? options) $ do
    case takeExtension in_file of
      ".traq" -> do
        p <- loadTraqProgram
        case eps of
          Just e -> lift $ compileCPL p e
          Nothing -> fail "--failprob is required when compiling .traq files"
      ".qpl" -> loadQPLProgram
      ext -> fail $ "Unsupported file extension: " ++ ext

  out_str <- case target of
    QPL -> emitQPL qpl_prog
    Qualtran -> emitQualtran qpl_prog
    Qiskit -> emitQiskit qpl_prog
  maybe putStr writeFile out_file out_str

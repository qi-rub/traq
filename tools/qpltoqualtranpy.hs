{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, execWriterT)
import Options.Applicative
import System.FilePath (takeExtension)
import Text.Read (readMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as Analysis
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Compiler.Qualtran
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- CLI
-- ============================================================

data Options = Options
  { in_file :: FilePath
  , out_file :: Maybe FilePath
  , eps :: Double
  , params :: [(Ident, SizeT)]
  }
  deriving (Show)

-- | Load a Traq program source, and substitute parameters.
loadTraqProgram :: ReaderT Options IO (CPL.Program (P.WorstCasePrims SizeT Double))
loadTraqProgram = do
  code <- lift . readFile =<< view (to in_file)
  case CPL.parseProgram @(P.WorstCasePrims _ Double) code of
    Left err -> fail $ show err
    Right prog -> do
      ps <- view (to params)
      pure $ CPL.mapSize (subs_params ps) prog
 where
  subs_params :: [(Ident, SizeT)] -> (Sym.Sym Int -> SizeT)
  subs_params params s = Sym.unSym $ foldr subsOnce s params
   where
    subsOnce :: (Ident, SizeT) -> Sym.Sym Int -> Sym.Sym Int
    subsOnce (k, v) = Sym.subst k (Sym.con v)

-- | Compiler source to target.
compile :: (RealFloat prec, Show prec) => CPL.Program (P.WorstCasePrims SizeT prec) -> prec -> IO (QPL.Program SizeT)
compile prog eps = do
  let prog_rn = if CPL.checkVarsUnique prog then prog else CPL.renameVars' prog
  prog' <- either fail pure $ Analysis.annotateProgWithErrorBudget (Analysis.failProb eps) prog_rn
  either fail pure $ Compiler.lowerProgram prog'

-- | Load a serialized QPL Program AST.
loadQPLProgram :: ReaderT Options IO (QPL.Program SizeT)
loadQPLProgram = do
  raw <- lift . readFile =<< view (to in_file)
  return $ read raw

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> header "Compile QPL programs to Qualtran Python.")
 where
  options =
    Options
      <$> strOption
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
      <*> option
        auto
        ( long "failprob"
            <> short 'p'
            <> metavar "FLOAT"
            <> help "The maximum failure probability of the entire program"
        )
      <*> many (option (maybeReader parseKeyValue) (long "arg" <> help "parameters..." <> metavar "NAME=VALUE"))

  parseKeyValue s = do
    let key = takeWhile (/= '=') s
    let valS = tail $ dropWhile (/= '=') s
    val <- readMaybe valS
    return (key, val)

main :: IO ()
main = do
  options@Options{..} <- execParser opts

  qpl_prog <- (runReaderT ?? options) $ do
    case takeExtension in_file of
      ".traq" -> do
        p <- loadTraqProgram
        lift $ compile p eps
      ".qpl" -> loadQPLProgram
      ext -> fail $ "Unsupported file extension: " ++ ext

  py_preamble <- readFile "tools/qualtran_prelude.py"
  let py_prog_str = toPy qpl_prog
  let py_postamble = ""

  let py_str = unlines [py_preamble, py_prog_str, py_postamble]
  maybe putStr writeFile out_file py_str

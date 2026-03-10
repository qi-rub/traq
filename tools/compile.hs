{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Options.Applicative
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.Compiler.Quantum as CompileQ
import Traq.Prelude
import Traq.Primitives
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

type SymbSize = Sym.Sym Int

data Options = Options
  { in_file :: FilePath
  , out_file :: FilePath
  , eps :: Maybe Double
  , params :: [(Ident, SizeT)]
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    ( fullDesc
        <> header "Compile CPL programs to QPL and analyze their costs."
    )
 where
  options =
    Options
      <$> strOption (long "input" <> short 'i' <> metavar "FILENAME" <> help "Input file")
      <*> strOption (long "output" <> short 'o' <> metavar "FILENAME" <> help "Output file")
      <*> optional
        ( option
            auto
            ( long "failprob"
                <> short 'p'
                <> metavar "FLOAT"
                <> help "The max. failure probability of the whole lowered the whole program"
            )
        )
      <*> many (option (maybeReader parseKeyValue) (long "arg" <> help "parameters..." <> metavar "NAME=VALUE"))

  parseKeyValue s = do
    let key = takeWhile (/= '=') s
    let valS = tail $ dropWhile (/= '=') s
    val <- readMaybe valS
    return (key, val)

subsNM :: [(Ident, SizeT)] -> (SymbSize -> SizeT)
subsNM params s = Sym.unSym $ foldr subsOnce s params
 where
  subsOnce :: (Ident, SizeT) -> SymbSize -> SymbSize
  subsOnce (k, v) = Sym.subst k (Sym.con v)

compile :: (RealFloat prec, Show prec) => CPL.Program (WorstCasePrims SizeT prec) -> prec -> IO String
compile prog eps = do
  let prog_rn = if CPL.checkVarsUnique prog then prog else CPL.renameVars' prog
  prog' <- either fail pure $ A.annotateProgWithErrorBudget (A.failProb eps) prog_rn
  cqpl_prog <- either fail pure $ CompileQ.lowerProgram prog'
  let nqubits = QPL.numQubits cqpl_prog

  return $ PP.toCodeString cqpl_prog ++ printf "\n// qubits: %d\n" nqubits

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  prog <- either (fail . show) (pure . CPL.mapSize (subsNM params)) $ CPL.parseProgram @(WorstCasePrims _ Double) code

  -- compile
  out_prog <- case eps of
    Just p -> compile prog p
    Nothing -> fail "compile prog (Sym.var \"d\" :: Sym.Sym Float)"
  writeFile out_file out_prog

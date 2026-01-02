{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Options.Applicative
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as P
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Quantum as CompileQ
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

type SymbSize = Sym.Sym Int

data Options = Options
  { in_file :: FilePath
  , out_file :: FilePath
  , eps :: Maybe Float
  , params :: [(Ident, SizeT)]
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    ( fullDesc
        <> header "Compile Proto programs to CQPL and analyze their costs."
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

compile :: (RealFloat precT, Show precT) => P.Program (DefaultPrims SizeT precT) -> precT -> IO String
compile prog eps = do
  Right cqpl_prog <- return $ CompileQ.lowerProgram default_ Ctx.empty (P.failProb eps) prog
  let nqubits = CQPL.numQubits cqpl_prog

  return $ PP.toCodeString cqpl_prog ++ printf "\n// qubits: %d\n" nqubits

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  Right prog <-
    return $
      P.mapSize (subsNM params) <$> P.parseProgram @(DefaultPrims _ _) code

  -- compile
  out_prog <- case eps of
    Just p -> compile prog p
    Nothing -> fail "compile prog (Sym.var \"d\" :: Sym.Sym Float)"
  writeFile out_file out_prog

module Main (main) where

import qualified Data.Map as Map
import Options.Applicative
import Text.Read (readMaybe)

import qualified QCompose.Data.Context as Ctx
import QCompose.Data.Default
import qualified QCompose.Data.Symbolic as Sym

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

import QCompose.Primitives (DefaultPrims)

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
        <> header "Compile Proto programs to UQPL and analyze their costs."
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

compile :: (RealFloat costT, Show costT) => P.Program DefaultPrims SizeT -> costT -> IO String
compile prog eps = do
  let oracle_ticks = Map.singleton "Oracle" 1.0
  let Right (cqpl_prog, _) = CQPL.lowerProgram default_ Ctx.empty oracle_ticks oracle_ticks eps prog

  return $ toCodeString cqpl_prog

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  let Right prog = fmap (subsNM params) <$> P.parseProgram code

  -- compile
  out_prog <- case eps of
    Just p -> compile prog p
    Nothing -> fail "compile prog (Sym.var \"d\" :: Sym.Sym Float)"
  writeFile out_file out_prog

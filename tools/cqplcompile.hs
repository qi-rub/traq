{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Number.Symbolic as Sym
import Options.Applicative
import Text.Read (readMaybe)

import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import QCompose.Primitives.QSearch
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

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
      <*> many (option (maybeReader parseKeyValue) (long "arg" <> help "paramters..." <> metavar "NAME=VALUE"))

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

compile :: (RealFloat costT, Show costT) => P.Program SizeT -> costT -> IO String
compile prog eps = do
  let Right (cqpl_prog, _) = CQPL.lowerProgram qSearchCQImpl Ctx.empty eps prog

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
    Nothing -> compile prog (Sym.var "d" :: Sym.Sym Float)
  writeFile out_file out_prog

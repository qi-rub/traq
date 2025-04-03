module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer (execWriterT, tell)
import qualified Data.Number.Symbolic as Sym
import Lens.Micro
import Options.Applicative
import Text.Read (readMaybe)

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Primitives.QSearch
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

data Options = Options
  { in_file :: FilePath
  , out_file :: FilePath
  , delta :: Maybe Float
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
            ( long "precision"
                <> short 'p'
                <> metavar "FLOAT"
                <> help "The precision to lower the whole program"
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

compile :: (RealFloat a, Show a) => P.Program SizeT -> a -> IO String
compile prog delta = do
  let Right (uqpl_prog, _) = UQPL.lowerProgram zalkaQSearch Ctx.empty delta prog
  -- get costs
  let (cost, proc_costs) = UQPL.programCost uqpl_prog

  -- print the program with the costs
  execWriterT $ do
    forM_ (uqpl_prog ^. to UQPL.proc_defs) $ \p -> do
      tell $ "// Cost: " <> show (proc_costs ^. at (p ^. to UQPL.proc_name) . singular _Just)
      tell "\n"
      tell $ toCodeString p
      tell "\n"

    tell $ "// Cost: " <> show cost
    tell "\n"
    tell $ toCodeString $ uqpl_prog ^. to UQPL.stmt
    tell "\n"

    tell $ "// Cost from formula: " ++ show (P.unitaryQueryCost cadeEtAlFormulas delta prog)

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  let Right prog = fmap (subsNM params) <$> P.parseProgram code

  -- compile
  out_prog <- case delta of
    Just d -> compile prog d
    Nothing -> compile prog (Sym.var "d" :: Sym.Sym Float)
  writeFile out_file out_prog

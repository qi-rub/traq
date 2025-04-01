module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer
import qualified Data.Number.Symbolic as Sym
import Lens.Micro
import Options.Applicative

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Primitives.QSearch
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

data Options = Options
  { in_file :: FilePath
  , out_file :: FilePath
  , delta :: Float
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
      <*> option
        auto
        ( long "precision"
            <> short 'p'
            <> metavar "FLOAT"
            <> help "The precision to lower the whole program"
        )

subsNM :: (SymbSize -> SizeT)
subsNM = Sym.unSym . Sym.subst "N" (Sym.con n) . Sym.subst "M" (Sym.con m)
 where
  n = 20
  m = 10

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  let Right prog = fmap subsNM <$> P.parseProgram code

  -- compile
  let Right (uqpl_prog, _) = UQPL.lowerProgram zalkaQSearch Ctx.empty delta prog
  -- get costs
  let (cost, proc_costs) = UQPL.programCost uqpl_prog

  -- print the program with the costs
  out_prog <- execWriterT $ do
    forM_ (uqpl_prog ^. to UQPL.proc_defs) $ \p -> do
      tell $ "// Cost: " <> show (proc_costs ^. at (p ^. to UQPL.proc_name) . singular _Just)
      tell "\n"
      tell $ toCodeString p
      tell "\n"

    tell $ "// Cost: " <> show cost
    tell "\n"
    tell $ toCodeString $ uqpl_prog ^. to UQPL.stmt
    tell "\n"

  writeFile out_file out_prog

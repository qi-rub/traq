module Main (main) where

import Control.Monad (forM_, guard, when)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Maybe (fromMaybe)
import Lens.Micro.GHC
import Options.Applicative
import Text.Read (readMaybe)

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Symbolic as Sym

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives (DefaultPrims)

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

tellLn :: (MonadWriter String m) => String -> m ()
tellLn x = tell $ unlines [x]

compile :: forall costT. (RealFloat costT, Show costT) => P.Program DefaultPrims SizeT -> costT -> IO String
compile prog delta = do
  let oracle_ticks = mempty & at "Oracle" ?~ (fromRational 1.0 :: costT)
  let Right (uqpl_prog, _) = UQPL.lowerProgram Ctx.empty oracle_ticks delta prog
  -- get costs
  let (cost :: costT, proc_costs) = UQPL.programCost uqpl_prog

  -- print the program with the costs
  execWriterT $ do
    forM_ (uqpl_prog ^. to UQPL.proc_defs . to Ctx.elems) $ \p -> do
      let pname = p ^. to UQPL.proc_name

      when (pname /= "Oracle") $ do
        let f_cost =
              fromMaybe
                "()"
                ( do
                    let fname = pname & takeWhile (/= '[')
                    let fdelta_s_suf = pname & dropWhile (/= '[') --
                    guard $ not $ null fdelta_s_suf
                    let fdelta_s = fdelta_s_suf & tail & takeWhile (/= ']')
                    fdelta <- readMaybe fdelta_s :: Maybe Double
                    P.FunDef{mbody = Just body} <- prog ^. to P.funCtx . Ctx.at fname
                    let cf =
                          P.unitaryQueryCost
                            fdelta
                            P.Program
                              { stmt = body ^. to P.body_stmt
                              , funCtx = prog ^. to P.funCtx
                              }
                            (mempty & at "Oracle" ?~ 1.0)
                    return $ show cf
                )

        let t_cost = proc_costs ^. at pname
        tellLn $ "// Cost         : " <> maybe "()" show t_cost
        tellLn $ "// Formula Cost : " <> f_cost
      tellLn $ toCodeString p

    tellLn $ "// Actual Cost : " <> show cost
    tellLn $ "// Formula Cost: " ++ show (P.unitaryQueryCost delta prog oracle_ticks)
    tellLn $ toCodeString $ uqpl_prog ^. to UQPL.stmt

main :: IO ()
main = do
  Options{..} <- execParser opts

  -- parse
  code <- readFile in_file
  prog <-
    P.parseProgram code
      & either (fail . show) return
      <&> fmap (subsNM params)

  -- compile
  out_prog <- case delta of
    Just d -> compile prog d
    Nothing -> fail "compile prog (Sym.var \"d\" :: Sym.Sym Float)"
  writeFile out_file out_prog

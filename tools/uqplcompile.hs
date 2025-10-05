module Main (main) where

import Control.Monad (forM_, guard, when)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Options.Applicative
import Text.Printf (printf)
import Text.Read (readMaybe)

import Lens.Micro.GHC
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Probability as Prob
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Unitary as CompileU
import Traq.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

type SymbSize = Sym.Sym Int

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
        <> header "Compile Proto programs to unitary fragment of CQPL and analyze their costs."
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

compile :: forall precT. (RealFloat precT, Show precT, Alg.Rig precT, Prob.ProbType precT, Prob.RVType precT precT) => P.Program DefaultPrims SizeT -> precT -> IO String
compile prog@(P.Program fs) delta = do
  Right cqpl_prog <- return $ CompileU.lowerProgram default_ Ctx.empty delta prog
  -- get costs
  let (_ :: SimpleQueryCost precT, proc_costs) = CQPL.programCost cqpl_prog

  -- print the program with the costs
  execWriterT $ do
    forM_ (cqpl_prog ^. to CQPL.proc_defs . to Ctx.elems) $ \p -> do
      let pname = p ^. to CQPL.proc_name

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
                    let prog' = P.Program $ dropWhileEnd (\f -> P.fun_name f /= fname) fs
                    let cf =
                          getCost $
                            P.unitaryQueryCost
                              P.SplitSimple
                              fdelta
                              prog'
                    return $ show cf
                )

        let t_cost = proc_costs ^. at pname
        tellLn $ "// Cost         : " <> maybe "()" (show . getCost) t_cost
        tellLn $ "// Formula Cost : " <> f_cost

      tellLn $ PP.toCodeString p

    tellLn $ printf "\n// qubits: %d\n" $ CQPL.numQubits cqpl_prog

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

module Main (main) where

import Control.Monad.Writer (MonadWriter, censor, execWriterT, listen, tell)

import qualified Traq.Analysis as Analysis
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

class ToQualtranPy a where
  py :: (MonadWriter [String] m, MonadFail m) => a -> m ()

main :: IO ()
main = error "TODO"

module Traq.Utils.Printing (
  ToCodeString (..),
  commaList,
  indent,
) where

import Control.Monad.Writer (MonadWriter, WriterT, censor, execWriter, listen, tell)
import Data.List (intercalate)
import Data.Void (Void, absurd)

class ToCodeString a where
  buildCode :: (MonadWriter [String] m) => a -> m ()
  buildCode = tell . toCodeLines

  toCodeString :: a -> String
  toCodeString = unlines . execWriter . buildCode

  toCodeLines :: a -> [String]
  toCodeLines = pure . toCodeString
  {-# MINIMAL toCodeString | toCodeLines | buildCode #-}

commaList :: [String] -> String
commaList = intercalate ", "

indent :: [String] -> [String]
indent = map ("  " <>)

instance ToCodeString Void where
  buildCode = absurd
  toCodeString = absurd
  toCodeLines = absurd

withIdent :: (MonadWriter [String] m) => m a -> m a
withIdent = censor indent

addLine :: (MonadWriter [a] m) => a -> m ()
addLine a = tell [a]

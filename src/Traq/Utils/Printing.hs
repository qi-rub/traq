module Traq.Utils.Printing (
  ToCodeString (..),
  indent,

  -- * Simple builders
  commaList,
  wrapNonEmpty,

  -- * Basic writers
  put,
  putLine,
  putComment,

  -- * Generic writer transformers
  mapped,
  joined,
  delimitedBlock,

  -- * Specific writer transformers
  indented,
  concatenated,
  unworded,
  unlined,
  commented,
  bracedBlock,
  bracedBlockWith,
  doEndBlock,
) where

import Control.Monad.Writer (MonadWriter, censor, execWriter, tell)
import Data.List (intercalate)
import Data.Void (Void, absurd)

-- Ideal Output state of the code writer.
-- data CodeBuilderState = CodeBuilderState {code_lines :: [String], current_line :: String}

class ToCodeString a where
  build :: (MonadWriter [String] m) => a -> m ()
  build = tell . toCodeLines

  toCodeString :: a -> String
  toCodeString = unlines . execWriter . build

  toCodeLines :: a -> [String]
  toCodeLines = pure . toCodeString
  {-# MINIMAL toCodeString | toCodeLines | build #-}

instance ToCodeString Void where
  build = absurd
  toCodeString = absurd
  toCodeLines = absurd

commaList :: [String] -> String
commaList = intercalate ", "

indent :: [String] -> [String]
indent = map ("  " <>)

-- | Wrap a non-empty string by the delimiters, and pass empty strings through as-is.
wrapNonEmpty :: String -> String -> String -> String
wrapNonEmpty _ _ "" = ""
wrapNonEmpty lt rt s = lt ++ s ++ rt

-- | Add a line of code.
putLine :: (MonadWriter [a] m) => a -> m ()
putLine a = tell [a]

put :: (MonadWriter [a] m) => a -> m ()
put a = tell [a]

-- | Map each line in a block of code.
mapped :: (MonadWriter [w] m) => (w -> w) -> m a -> m a
mapped f = censor (map f)

-- | Join a block of code using a joining function.
joined :: (MonadWriter [w] m) => ([w] -> w) -> m a -> m a
joined f = censor (pure . f)

-- | Indent the block of code.
indented :: (MonadWriter [String] m) => m a -> m a
indented = mapped ("  " <>)

-- | Concatenate the block of code into a single line.
concatenated :: (MonadWriter [[w]] m) => m a -> m a
concatenated = joined concat

-- | @unwords@ the lines in a block of code.
unworded :: (MonadWriter [String] m) => m a -> m a
unworded = joined unwords

-- | @unlines@ the lines in a block of code.
unlined :: (MonadWriter [String] m) => m a -> m a
unlined = joined unlines

commented :: (MonadWriter [String] m) => m a -> m a
commented = mapped ("// " <>)

delimitedBlock :: (MonadWriter [String] m) => String -> String -> m a -> m a
delimitedBlock st en m = putLine st *> indented m <* putLine en

bracedBlock :: (MonadWriter [String] m) => m a -> m a
bracedBlock = delimitedBlock "{" "}"

bracedBlockWith :: (MonadWriter [String] m) => String -> m a -> m a
bracedBlockWith header = delimitedBlock (header <> " {") "}"

doEndBlock :: (MonadWriter [String] m) => m a -> m a
doEndBlock = delimitedBlock "do" "end"

putComment :: (MonadWriter [String] m) => String -> m ()
putComment s = commented $ putLine s

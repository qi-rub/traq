module Traq.Utils.Printing (
  ToCodeString (..),
  toCodeString,

  -- * Simple builders
  commaList,
  wrapNonEmpty,

  -- * Basic writers
  putWord,
  putLine,
  putComment,
  endl,
  fromBuild,
  listenWord,

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
  prepended,
) where

import Control.Monad.Fail (MonadFail)
import Control.Monad.Writer (MonadWriter, censor, execWriterT, listen, tell)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)

-- Ideal Output state of the code writer.
-- data CodeBuilderState = CodeBuilderState {code_lines :: [String], current_line :: String}

class ToCodeString a where
  build :: (MonadWriter [String] m, MonadFail m) => a -> m ()

toCodeString :: (ToCodeString a) => a -> String
toCodeString = unlines . fromJust . execWriterT . build

fromBuild :: (ToCodeString a, MonadWriter [String] m, MonadFail m) => a -> m String
fromBuild a = censor (const mempty) $ do
  (_, [line]) <- listen $ build a
  return line

listenWord :: (MonadWriter [String] m, MonadFail m) => m () -> m String
listenWord m = censor (const mempty) $ do
  ((), ws) <- listen m
  return $ concat ws

instance ToCodeString Void where build = absurd

commaList :: [String] -> String
commaList = intercalate ", "

-- | Wrap a non-empty string by the delimiters, and pass empty strings through as-is.
wrapNonEmpty :: String -> String -> String -> String
wrapNonEmpty _ _ "" = ""
wrapNonEmpty lt rt s = lt ++ s ++ rt

-- | Add a line of code.
putLine :: (MonadWriter [w] m) => w -> m ()
putLine w = tell [w]

endl :: (MonadWriter [String] m) => m ()
endl = putLine ""

putWord :: (MonadWriter [w] m) => w -> m ()
putWord w = tell [w]

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
putComment "" = return ()
putComment s = commented $ putLine s

-- | Prepend the string to the first line out the output
prepended :: (MonadWriter [String] m) => String -> m a -> m a
prepended header = censor prep
 where
  prep [] = [header]
  prep (l : ls) = (header ++ l) : ls

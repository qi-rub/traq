module QCompose.Utils.Printing where

import Data.List (intercalate)

class ToCodeString a where
  toCodeString :: a -> String
  toCodeString = unlines . toCodeLines

  toCodeLines :: a -> [String]
  toCodeLines = pure . toCodeString

commaList :: [String] -> String
commaList = intercalate ", "

indent :: [String] -> [String]
indent = map ("  " <>)

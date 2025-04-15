module QCompose.Utils.Printing where

import Data.List (intercalate)
import Data.Void (Void, absurd)

class ToCodeString a where
  toCodeString :: a -> String
  toCodeString = unlines . toCodeLines

  toCodeLines :: a -> [String]
  toCodeLines = pure . toCodeString
  {-# MINIMAL toCodeString | toCodeLines #-}

commaList :: [String] -> String
commaList = intercalate ", "

indent :: [String] -> [String]
indent = map ("  " <>)

instance ToCodeString Void where
  toCodeString = absurd
  toCodeLines = absurd

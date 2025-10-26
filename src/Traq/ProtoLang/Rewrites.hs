module Traq.ProtoLang.Rewrites where

import Traq.ProtoLang.Syntax

-- | Flatten nested 'SeqS' instructions
flattenSeq :: Stmt ext -> Maybe (Stmt ext)
flattenSeq (SeqS [s]) = Just s
flattenSeq (SeqS ss)
  | any isSeq ss =
      Just . fromList . concatMap toList $ ss
 where
  isSeq :: Stmt ext -> Bool
  isSeq (SeqS _) = True
  isSeq _ = False

  toList :: Stmt ext -> [Stmt ext]
  toList (SeqS xs) = xs
  toList x = [x]

  fromList :: [Stmt ext] -> Stmt ext
  fromList [x] = x
  fromList xs = SeqS xs
flattenSeq _ = Nothing

{- | Convert all 'SeqS' into a list of size at most 2.
  For example, @SeqS [a, b, c]@ becomes @SeqS [a, SeqS [b, c]]@
-}
pairSeq :: Stmt ext -> Maybe (Stmt ext)
pairSeq (SeqS [s]) = Just s
pairSeq (SeqS ss@(_ : _ : _)) = Just $ foldr1 (\a b -> SeqS [a, b]) ss
pairSeq _ = Nothing

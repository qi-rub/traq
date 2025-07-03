module Traq.ProtoLang.Rewrites where

import Traq.ProtoLang.Syntax

-- | Flatten nested 'SeqS' instructions
flattenSeq :: Stmt primT sizeT -> Maybe (Stmt primT sizeT)
flattenSeq (SeqS [s]) = Just s
flattenSeq (SeqS ss)
  | any isSeq ss =
      Just . fromList . concatMap toList $ ss
 where
  isSeq :: Stmt primT sizeT -> Bool
  isSeq (SeqS _) = True
  isSeq _ = False

  toList :: Stmt primT sizeT -> [Stmt primT sizeT]
  toList (SeqS xs) = xs
  toList x = [x]

  fromList :: [Stmt primT sizeT] -> Stmt primT sizeT
  fromList [x] = x
  fromList xs = SeqS xs
flattenSeq _ = Nothing

{- | Convert all 'SeqS' into a list of size at most 2.
  For example, @SeqS [a, b, c]@ becomes @SeqS [a, SeqS [b, c]]@
-}
pairSeq :: Stmt primT sizeT -> Maybe (Stmt primT sizeT)
pairSeq (SeqS [s]) = Just s
pairSeq (SeqS ss@(_ : _ : _)) = Just $ foldr1 (\a b -> SeqS [a, b]) ss
pairSeq _ = Nothing

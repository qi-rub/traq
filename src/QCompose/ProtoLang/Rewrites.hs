module QCompose.ProtoLang.Rewrites where

import QCompose.ProtoLang.Syntax

flattenSeq :: Stmt a -> Maybe (Stmt a)
flattenSeq (SeqS [s]) = Just s
flattenSeq (SeqS ss)
  | any isSeq ss =
      Just . fromList . concatMap toList $ ss
  where
    isSeq :: Stmt a -> Bool
    isSeq (SeqS _) = True
    isSeq _ = False

    toList :: Stmt a -> [Stmt a]
    toList (SeqS xs) = xs
    toList x = [x]

    fromList :: [Stmt a] -> Stmt a
    fromList [x] = x
    fromList xs = SeqS xs
flattenSeq _ = Nothing

pairSeq :: Stmt a -> Stmt a
pairSeq (SeqS ss) | not (null ss) = foldr1 (\a b -> SeqS [a, b]) ss
pairSeq s = s

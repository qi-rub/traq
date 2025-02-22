module QCompose.ProtoLang.Rewrites where

import QCompose.ProtoLang.Syntax

flattenSeq :: Stmt a -> Stmt a
flattenSeq (SeqS ss) = case concatMap toSeq ss of
  [s] -> s
  ss' -> SeqS ss'
  where
    toSeq :: Stmt a -> [Stmt a]
    toSeq (SeqS xs) = xs
    toSeq x = [x]
flattenSeq s = s

pairSeq :: Stmt a -> Stmt a
pairSeq (SeqS ss) | not (null ss) = foldr1 (\a b -> SeqS [a, b]) ss
pairSeq s = s

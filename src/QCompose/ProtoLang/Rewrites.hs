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

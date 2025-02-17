module QCompose.ProtoLang.Rewrites where

import QCompose.ProtoLang.Syntax

flattenSeq :: Stmt -> Stmt
flattenSeq (SSeq ss) = case concatMap toSeq ss of
  [s] -> s
  ss' -> SSeq ss'
  where
    toSeq :: Stmt -> [Stmt]
    toSeq (SSeq xs) = xs
    toSeq x = [x]
flattenSeq s = s
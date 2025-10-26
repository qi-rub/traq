module Traq.Utils.ASTRewriting (
  HasAst (..),
  HasStmt (..),
  rewriteAST,
) where

import Lens.Micro.GHC

-- | Typeclass for the `ast` lens, which looks at immediate children
class HasAst a where
  _ast :: Traversal' a a

-- | Typeclass for lifting underlying ASTs.
class HasStmt p where
  type StmtOf p

  _stmt :: Traversal' p (StmtOf p)

-- | Rewrite an AST locally.
rewriteAST :: (s ~ StmtOf p, HasAst s, HasStmt p) => (s -> Maybe s) -> p -> p
rewriteAST rw = _stmt %~ rewriteOf _ast rw

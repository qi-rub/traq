{-# LANGUAGE FunctionalDependencies #-}

module QCompose.Utils.ASTRewriting (
  HasAst (..),
  HasStmt (..),
  rewriteAST,
) where

import Lens.Micro

-- Typeclass for the `ast` lens, which looks at immediate children
class HasAst a where
  _ast :: Traversal' a a

-- Typeclass for lifting underlying ASTs.
class (HasAst s) => HasStmt p s | p -> s where
  _stmt :: Traversal' p s

-- Rewrite an AST locally.
rewriteAST :: (HasStmt p s) => (s -> Maybe s) -> p -> p
rewriteAST rw = _stmt %~ rewriteOf _ast rw

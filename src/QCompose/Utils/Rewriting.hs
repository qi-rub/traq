module QCompose.Utils.Rewriting where

class LocalRewritable a where
  rewrite :: (Monad m) => (a -> m a) -> a -> m a

  rewriteS :: (Monad m) => (a -> a) -> a -> m a
  rewriteS = rewrite . (pure .)

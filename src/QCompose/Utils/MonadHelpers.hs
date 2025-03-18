{-# LANGUAGE Rank2Types #-}

module QCompose.Utils.MonadHelpers (
  withSandbox,
  withSandboxOf,
  withFrozenState,
  embedStateT,
  embedReaderT,
  embedWriterT,
) where

import Control.Monad.RWS (RWST (..))
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..))
import Lens.Micro
import Lens.Micro.Mtl

-- | Save the current state, run a computation and restore the saved state.
withSandbox :: (MonadState s m) => m a -> m a
withSandbox m = do
  s <- get
  a <- m
  put s
  return a

-- | Save the current state, run a computation and restore the saved state.
withSandboxOf :: (MonadState s m) => Lens' s s' -> m a -> m a
withSandboxOf part action = do
  s <- use part
  a <- action
  part .= s
  return a

-- | Run a computation with the current state as a read-only environment.
withFrozenState :: (Monad m) => ReaderT s m a -> StateT s m a
withFrozenState m = do
  s <- get
  lift $ runReaderT m s

embedStateT :: (Monad m, Monoid w) => StateT s m a -> RWST r w s m a
embedStateT m = RWST $ \_ s -> do
  (a, s') <- runStateT m s
  return (a, s', mempty)

embedReaderT :: (Monad m, Monoid w) => ReaderT r m a -> RWST r w s m a
embedReaderT m = RWST $ \r s -> do
  a <- runReaderT m r
  return (a, s, mempty)

embedWriterT :: (Monad m, Monoid w) => WriterT w m a -> RWST r w s m a
embedWriterT m = RWST $ \_ s -> do
  (a, w) <- runWriterT m
  return (a, s, w)

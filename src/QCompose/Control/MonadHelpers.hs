{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module QCompose.Control.MonadHelpers (
  withSandboxOf,
  withSandbox,
  withFrozenStateOf,
  withFrozenState,
  embedStateT,
  embedReaderT,
  embedWriterT,
  throwFrom,
  maybeWithError,
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.RWS (RWST (..))
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Control.Monad.State (MonadState, StateT, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..))
import Lens.Micro
import Lens.Micro.Mtl

-- | Save the current state, run a computation and restore the saved state.
withSandboxOf :: (MonadState s m) => Lens' s s' -> m a -> m a
withSandboxOf part action = do
  s <- use part
  a <- action
  part .= s
  return a

-- | Save the current state, run a computation and restore the saved state.
withSandbox :: (MonadState s m) => m a -> m a
withSandbox = withSandboxOf id

-- | Run a computation with the current state as a read-only environment.
withFrozenStateOf :: (Monad m) => Lens' s s' -> ReaderT s' m a -> StateT s m a
withFrozenStateOf part m = do
  s <- use part
  lift $ runReaderT m s

-- | Run a computation with the current state as a read-only environment.
withFrozenState :: (Monad m) => ReaderT s m a -> StateT s m a
withFrozenState = withFrozenStateOf id

-- | Embed a state computation into an RWS monad.
embedStateT :: (Monad m, Monoid w) => StateT s m a -> RWST r w s m a
embedStateT m = RWST $ \_ s -> do
  (a, s') <- runStateT m s
  return (a, s', mempty)

-- | Embed a reader computation into an RWS monad.
embedReaderT :: (Monad m, Monoid w) => ReaderT r m a -> RWST r w s m a
embedReaderT m = RWST $ \r s -> do
  a <- runReaderT m r
  return (a, s, mempty)

-- | Embed a writer computation into an RWS monad.
embedWriterT :: (Monad m, Monoid w) => WriterT w m a -> RWST r w s m a
embedWriterT m = RWST $ \_ s -> do
  (a, w) <- runWriterT m
  return (a, s, w)

-- | try-catch block that prepends a message to the existing error
throwFrom :: (MonadError String m) => m a -> String -> m a
throwFrom action msg =
  action `catchError` \e ->
    throwError $ unlines [msg, "caught while handling exception:", e]

-- | lift a @Maybe@ to a value, throwing an error if @Nothing@
maybeWithError :: (MonadError e m) => e -> Maybe a -> m a
maybeWithError err = maybe (throwError err) return

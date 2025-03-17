module QCompose.Utils.MonadHelpers (withSandbox, withFrozenState) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, get, put)
import Control.Monad.Trans (lift)

-- | Save the current state, run a computation and restore the saved state.
withSandbox :: (MonadState s m) => m a -> m a
withSandbox m = do
  s <- get
  a <- m
  put s
  return a

-- | Run a computation with the current state as a read-only environment.
withFrozenState :: (Monad m) => ReaderT s m a -> StateT s m a
withFrozenState m = do
  s <- get
  lift $ runReaderT m s

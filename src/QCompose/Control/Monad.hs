module QCompose.Control.Monad (
  -- * Monad Types
  MyReaderWriterStateT,

  -- ** Reader
  MyReaderT,
  runMyReaderT,

  -- ** State
  MyStateT,
  runMyStateT,
  evalMyStateT,
  execMyStateT,

  -- ** Writer
  MyWriterT,
  runMyWriterT,
  tellAt,

  -- ** RW
  MyReaderStateT,

  -- * functions
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
import Control.Monad.RWS (MonadState, MonadWriter, RWST (..), evalRWST, runRWST, tell)
import Control.Monad.Trans (lift)
import Lens.Micro
import Lens.Micro.Mtl

-- | Overly descriptive alias for RWS
type MyReaderWriterStateT = RWST

-- | Reader type using RWS
type MyReaderT r = MyReaderWriterStateT r () ()

runMyReaderT :: (Monad m) => MyReaderT r m a -> r -> m a
runMyReaderT rws r = do
  (a, (), ()) <- runRWST rws r ()
  return a

-- | Writer type using RWS
type MyWriterT w = MyReaderWriterStateT () w ()

runMyWriterT :: (Monad m, Monoid w) => MyWriterT w m a -> m (a, w)
runMyWriterT rws = evalRWST rws () ()

tellAt :: (MonadWriter w m) => Lens' w w' -> w' -> m ()
tellAt focus w' =
  let w = mempty & focus .~ w'
   in tell w

-- | State type using RWS
type MyStateT s = MyReaderWriterStateT () () s

runMyStateT :: (Monad m) => MyStateT s m a -> s -> m (a, s)
runMyStateT rws s = do
  (a, s', ()) <- runRWST rws () s
  return (a, s')

evalMyStateT :: (Monad m) => MyStateT s m a -> s -> m a
evalMyStateT rws s = fst <$> runMyStateT rws s

execMyStateT :: (Monad m) => MyStateT s m a -> s -> m s
execMyStateT rws s = snd <$> runMyStateT rws s

-- | Reader+State monad using RWS
type MyReaderStateT r s = MyReaderWriterStateT r () s

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
withFrozenStateOf :: (Monad m) => Lens' s s' -> MyReaderT s' m a -> MyStateT s m a
withFrozenStateOf part m = do
  s <- use part
  (a, (), ()) <- lift $ runRWST m s ()
  return a

-- | Run a computation with the current state as a read-only environment.
withFrozenState :: (Monad m) => MyReaderT s m a -> MyStateT s m a
withFrozenState = withFrozenStateOf id

-- | Embed a state computation into an RWS monad.
embedStateT :: (Monad m, Monoid w) => MyStateT s m a -> MyReaderWriterStateT r w s m a
embedStateT m = RWST $ \_ s -> do
  (a, s', ()) <- runRWST m () s
  return (a, s', mempty)

-- | Embed a reader computation into an RWS monad.
embedReaderT :: (Monad m, Monoid w) => MyReaderT r m a -> MyReaderWriterStateT r w s m a
embedReaderT m = RWST $ \r s -> do
  (a, (), ()) <- runRWST m r ()
  return (a, s, mempty)

-- | Embed a writer computation into an RWS monad.
embedWriterT :: (Monad m, Monoid w) => MyWriterT w m a -> MyReaderWriterStateT r w s m a
embedWriterT m = RWST $ \_ s -> do
  (a, (), w) <- runRWST m () ()
  return (a, s, w)

-- | try-catch block that prepends a message to the existing error
throwFrom :: (MonadError String m) => m a -> String -> m a
throwFrom action msg =
  action `catchError` \e ->
    throwError $ unlines [msg, "caught while handling exception:", e]

-- | lift a @Maybe@ to a value, throwing an error if @Nothing@
maybeWithError :: (MonadError e m) => e -> Maybe a -> m a
maybeWithError err = maybe (throwError err) return

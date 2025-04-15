module QCompose.Control.Monad (
  -- * Monad Types
  MyReaderWriterStateT,
  runMyReaderWriterStateT,
  evalMyReaderWriterStateT,
  execMyReaderWriterStateT,

  -- ** Reader
  MyReaderT,
  runMyReaderT,
  embedReaderT,
  withFrozenStateOf,
  withFrozenState,

  -- ** State
  MyStateT,
  runMyStateT,
  evalMyStateT,
  execMyStateT,
  embedStateT,
  withSandboxOf,
  withSandbox,

  -- ** Writer
  MyWriterT,
  runMyWriterT,
  tellAt,
  embedWriterT,
  censored,

  -- ** RS
  MyReaderStateT,
  runMyReaderStateT,
  evalMyReaderStateT,
  execMyReaderStateT,
  withInjectedState,

  -- * MonadError
  throwFrom,
  maybeWithError,
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.RWS (
  MonadState,
  MonadWriter,
  RWST (..),
  evalRWST,
  execRWST,
  mapRWST,
  runRWST,
  tell,
 )
import Control.Monad.Trans (lift)
import Lens.Micro.GHC
import Lens.Micro.Mtl

-- ================================================================================
-- RWS
-- ================================================================================

-- | Overly descriptive alias for RWS
type MyReaderWriterStateT = RWST

runMyReaderWriterStateT :: (Monad m) => MyReaderWriterStateT r w s m a -> r -> s -> m (a, s, w)
runMyReaderWriterStateT = runRWST

evalMyReaderWriterStateT :: (Monad m) => MyReaderWriterStateT r w s m a -> r -> s -> m (a, w)
evalMyReaderWriterStateT = evalRWST

execMyReaderWriterStateT :: (Monad m) => MyReaderWriterStateT r w s m a -> r -> s -> m (s, w)
execMyReaderWriterStateT = execRWST

-- ================================================================================
-- Reader
-- ================================================================================

-- | Reader type using RWS
type MyReaderT r = MyReaderWriterStateT r () ()

-- | @runReaderT@ for @RWST@
runMyReaderT :: (Monad m) => MyReaderT r m a -> r -> m a
runMyReaderT rws r = do
  (a, (), ()) <- runRWST rws r ()
  return a

-- | Embed a reader computation into an RWS monad.
embedReaderT :: (Monad m, Monoid w) => MyReaderT r m a -> MyReaderWriterStateT r w s m a
embedReaderT rws = RWST $ \r s -> do
  (a, (), ()) <- runRWST rws r ()
  return (a, s, mempty)

-- ================================================================================
-- Writer
-- ================================================================================

-- | Writer type using RWS
type MyWriterT w = MyReaderWriterStateT () w ()

-- | @runWriterT@ for @RWST@
runMyWriterT :: (Monad m, Monoid w) => MyWriterT w m a -> m (a, w)
runMyWriterT rws = evalRWST rws () ()

-- | Write at a particular location in the monoid.
tellAt :: (MonadWriter w m) => Lens' w w' -> w' -> m ()
tellAt focus w' =
  let w = mempty & focus .~ w'
   in tell w

-- | Embed a writer computation into an RWS monad.
embedWriterT :: (Monad m, Monoid w) => MyWriterT w m a -> MyReaderWriterStateT r w s m a
embedWriterT rws = RWST $ \_ s -> do
  (a, (), w) <- runRWST rws () ()
  return (a, s, w)

-- | Ignore the writer output
censored :: (Monad m, Monoid w) => RWST r () s m a -> RWST r w s m a
censored = mapRWST $ fmap (_3 .~ mempty)

-- ================================================================================
-- State
-- ================================================================================

-- | State type using RWS
type MyStateT s = MyReaderWriterStateT () () s

-- | @runStateT@ for @RWST@
runMyStateT :: (Monad m) => MyStateT s m a -> s -> m (a, s)
runMyStateT rws s = do
  (a, s', ()) <- runRWST rws () s
  return (a, s')

-- | @evalStateT@ for @RWST@
evalMyStateT :: (Monad m) => MyStateT s m a -> s -> m a
evalMyStateT rws s = fst <$> runMyStateT rws s

-- | @execStateT@ for @RWST@
execMyStateT :: (Monad m) => MyStateT s m a -> s -> m s
execMyStateT rws s = snd <$> runMyStateT rws s

-- | Embed a state computation into an RWS monad.
embedStateT :: (Monad m, Monoid w) => MyStateT s m a -> MyReaderWriterStateT r w s m a
embedStateT m = RWST $ \_ s -> do
  (a, s', ()) <- runRWST m () s
  return (a, s', mempty)

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

-- ================================================================================
-- Reader+State
-- ================================================================================

-- | Reader+State monad using RWS
type MyReaderStateT r s = MyReaderWriterStateT r () s

runMyReaderStateT :: (Monad m) => MyReaderStateT r s m a -> r -> s -> m (a, s)
runMyReaderStateT m r s = do
  (a, s', ()) <- runRWST m r s
  return (a, s')

evalMyReaderStateT :: (Monad m) => MyReaderStateT r s m a -> r -> s -> m a
evalMyReaderStateT = ((fmap fst .) .) . runMyReaderStateT

execMyReaderStateT :: (Monad m) => MyReaderStateT r s m a -> r -> s -> m s
execMyReaderStateT = ((fmap snd .) .) . runMyReaderStateT

-- | Run a computation with the current state as a read-only environment.
withFrozenStateOf :: (Monad m) => Lens' s s' -> MyReaderT s' m a -> MyStateT s m a
withFrozenStateOf part m = do
  s <- use part
  (a, (), ()) <- lift $ runRWST m s ()
  return a

-- | Run a computation with the current state as a read-only environment.
withFrozenState :: (Monad m) => MyReaderT s m a -> MyStateT s m a
withFrozenState = withFrozenStateOf id

withInjectedState :: forall r w s s' m a. (Monad m, Monoid w) => s -> MyReaderWriterStateT r w s m a -> MyReaderWriterStateT r w s' m a
withInjectedState s = zoom _s_lens
 where
  -- simply gets `s`, and ignores while setting.
  _s_lens :: Lens' s' s
  _s_lens = lens (const s) const

-- ================================================================================
-- MonadError
-- ================================================================================

-- | try-catch block that prepends a message to the existing error, to produce a more verbose backtrace.
throwFrom :: (MonadError String m) => m a -> String -> m a
throwFrom action msg =
  action `catchError` \e ->
    throwError $ unlines [msg, "caught while handling exception:", e]

-- | lift a @Maybe@ to a value, throwing an error if @Nothing@
maybeWithError :: (MonadError e m) => e -> Maybe a -> m a
maybeWithError err = maybe (throwError err) return

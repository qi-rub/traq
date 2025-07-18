module Traq.Control.Monad (
  -- * Monad Types
  MyReaderWriterStateT,
  runMyReaderWriterStateT,

  -- ** Reader
  MyReaderT,
  runMyReaderT,

  -- ** State
  withSandboxOf,
  withSandbox,

  -- ** Writer
  tell,
  tellAt,
  writeElemAt,
  writeElem,
  censored,

  -- ** RS
  MyReaderStateT,
  runMyReaderStateT,
  evalMyReaderStateT,
  execMyReaderStateT,

  -- ** RW
  MyReaderWriterT,
  evalMyReaderWriterT,
  execMyReaderWriterT,

  -- * MonadError
  throwFrom,
  singularM,
  maybeWithError,

  -- * Helpers
  (??),
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.RWS (
  MonadState,
  MonadWriter,
  RWST (..),
  evalRWST,
  mapRWST,
  runRWST,
  tell,
 )
import Data.Monoid (Endo)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Data.Errors

-- ================================================================================
-- RWS
-- ================================================================================

-- | Overly descriptive alias for RWS
type MyReaderWriterStateT = RWST

runMyReaderWriterStateT :: (Monad m) => MyReaderWriterStateT r w s m a -> r -> s -> m (a, s, w)
runMyReaderWriterStateT = runRWST

evalMyReaderWriterStateT :: (Monad m) => MyReaderWriterStateT r w s m a -> r -> s -> m (a, w)
evalMyReaderWriterStateT = evalRWST

-- ================================================================================
-- Reader+State
-- ================================================================================

-- | Reader+State monad using RWS
type MyReaderStateT r s = MyReaderWriterStateT r () s

runMyReaderStateT :: (Monad m, Monoid w) => MyReaderWriterStateT r w s m a -> r -> s -> m (a, s)
runMyReaderStateT m r s = do
  (a, s', _) <- runRWST m r s
  return (a, s')

evalMyReaderStateT :: (Monad m, Monoid w) => MyReaderWriterStateT r w s m a -> r -> s -> m a
evalMyReaderStateT = ((fmap fst .) .) . runMyReaderStateT

execMyReaderStateT :: (Monad m, Monoid w) => MyReaderWriterStateT r w s m a -> r -> s -> m s
execMyReaderStateT = ((fmap snd .) .) . runMyReaderStateT

-- ================================================================================
-- Reader
-- ================================================================================

-- | Reader type using RWS
type MyReaderT r = MyReaderWriterStateT r () ()

-- | @runReaderT@ for @RWST@
runMyReaderT :: (Monad m) => MyReaderT r m a -> r -> m a
runMyReaderT rws r = evalMyReaderStateT rws r ()

-- ================================================================================
-- Writer
-- ================================================================================

-- | Write at a particular location in the monoid.
tellAt :: (MonadWriter w m) => Lens' w w' -> w' -> m ()
tellAt focus w' =
  let w = mempty & focus .~ w'
   in tell w

writeElemAt :: (Applicative f, MonadWriter w m) => Lens' w (f a) -> a -> m ()
writeElemAt focus = tellAt focus . pure

writeElem :: (Applicative f, MonadWriter (f a) m) => a -> m ()
writeElem = writeElemAt id

-- | Ignore the writer output
censored :: (Monad m, Monoid w) => RWST r () s m a -> RWST r w s m a
censored = mapRWST $ fmap (_3 .~ mempty)

-- ================================================================================
-- State
-- ================================================================================

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
-- Reader + Writer
-- ================================================================================
type MyReaderWriterT r w = MyReaderWriterStateT r w ()

evalMyReaderWriterT :: (Monad m, Monoid w) => r -> MyReaderWriterT r w m a -> m a
evalMyReaderWriterT r m = fst <$> evalMyReaderWriterStateT m r ()

execMyReaderWriterT :: (Monad m, Monoid w) => r -> MyReaderWriterT r w m a -> m w
execMyReaderWriterT r m = snd <$> evalMyReaderWriterStateT m r ()

-- ================================================================================
-- MonadError
-- ================================================================================

-- | try-catch block that prepends a message to the existing error, to produce a more verbose backtrace.
throwFrom :: (MonadError MyError m) => m a -> MyError -> m a
throwFrom action msg = action `catchError` (throwError . CatchE msg)

-- | extract a singular value from a traversal, throwing an error if there are 0/more than 1.
singularM :: (MonadError e m) => Getting (Endo [a]) s a -> e -> s -> m a
singularM l err s = case s ^.. l of [a] -> pure a; _ -> throwError err

-- | lift a @Maybe@ to a value, throwing an error if @Nothing@
maybeWithError :: (MonadError e m) => e -> Maybe a -> m a
maybeWithError = singularM _Just

-- ================================================================================
-- Helpers
-- ================================================================================

(??) :: (Functor f) => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

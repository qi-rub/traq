{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Control.Monad (
  -- * Traversable
  mapAccumM,
  forAccumM,

  -- * MonadState
  withSandboxOf,
  withSandbox,

  -- * MonadWriter
  tell,
  tellAt,
  writeElemAt,
  writeElem,

  -- * MonadError
  throwFrom,
  singularM,
  maybeWithError,
  liftEither,

  -- * Helpers
  (??),
  non',
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.RWS (MonadState, MonadWriter, tell)
import Control.Monad.State (StateT (..), runStateT)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo)
import Data.Tuple (swap)

#if __GLASGOW_HASKELL__ >= 906
import Data.Traversable(forAccumM,mapAccumM)
#endif

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Data.Errors

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

#if __GLASGOW_HASKELL__ < 906

-- | https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Traversable.html#v:mapAccumM
mapAccumM ::
  forall m t s a b.
  (Monad m, Traversable t) =>
  (s -> a -> m (s, b)) ->
  s ->
  t a ->
  m (s, t b)
mapAccumM f s t = swap <$> runStateT ((mapM @t @(StateT s m) @a @b) f' t) s
 where
  f' a = StateT $ fmap swap . flip f a

-- | https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Traversable.html#v:forAccumM
forAccumM ::
  (Monad m, Traversable t) =>
  s ->
  t a ->
  (s -> a -> m (s, b)) ->
  m (s, t b)
{-# INLINE forAccumM #-}
forAccumM s t f = mapAccumM f s t

#endif

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

-- | lift a @Maybe@ to a value, throwing an error if @Nothing@
liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right a) = pure a

-- ================================================================================
-- Helpers
-- ================================================================================

(??) :: (Functor f) => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

-- | Getter to set @Nothing@ to a default value.
non' :: a -> SimpleGetter (Maybe a) a
non' a = to (fromMaybe a)

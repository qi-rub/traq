{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module QCompose.Data.Context (
  Context,
  at,
  empty,
  null,
  (\\),
  fromList,
  singleton,
  toList,
  elems,
  lookup,
  lookup',
  put,
) where

import Prelude hiding (lookup, null)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import qualified Data.Map as Map
import Lens.Micro hiding (at)
import qualified Lens.Micro as Lens
import Lens.Micro.GHC ()
import Lens.Micro.Mtl
import Text.Printf (printf)

type Ident = String

newtype Context a = Context (Map.Map Ident a)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

unCtx :: Lens' (Context a) (Map.Map Ident a)
unCtx f (Context m) = Context <$> f m

at :: Ident -> Lens' (Context a) (Maybe a)
at k = unCtx . Lens.at k

empty :: Context a
empty = Context Map.empty

null :: Context a -> Bool
null (Context m) = Map.null m

(\\) :: Context a -> Context a -> Context a
(Context m) \\ (Context m') = Context (m Map.\\ m')

fromList :: [(Ident, a)] -> Context a
fromList = Context . Map.fromList

singleton :: Ident -> a -> Context a
singleton k v = fromList [(k, v)]

toList :: Context a -> [(Ident, a)]
toList = view (unCtx . to Map.assocs)

elems :: Context a -> [a]
elems = map snd . toList

lookup :: (MonadError String m, MonadState (Context a) m) => Ident -> m a
lookup x = do
  v <- use (at x)
  maybe (throwError $ "cannot find variable " <> show x) pure v

lookup' :: (MonadError String m, MonadReader (Context a) m) => Ident -> m a
lookup' x = do
  v <- view (at x)
  maybe (throwError $ "cannot find variable " <> show x) pure v

put :: (MonadError String m, MonadState (Context a) m) => Ident -> a -> m ()
put x v = do
  exists <- use (unCtx . to (Map.member x))
  if exists
    then throwError (printf "variable `%s` already exists!" x)
    else at x ?= v

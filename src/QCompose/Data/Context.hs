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
  CanFail (..),
  lookup,
  lookup',
  put,
) where

import Prelude hiding (lookup, null)

import qualified Control.Applicative as Ap
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Map as Map
import Lens.Micro hiding (at)
import qualified Lens.Micro as Lens
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import qualified QCompose.Data.Tree as Tree

type Ident = String

newtype Context a = Context (Map.Map Ident a)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

ctx :: Lens' (Context a) (Map.Map Ident a)
ctx f (Context m) = Context <$> f m

at :: Ident -> Lens' (Context a) (Maybe a)
at k = ctx . Lens.at k

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
toList = view (ctx . to Map.assocs)

elems :: Context a -> [a]
elems = map snd . toList

class (Monad m) => CanFail m where
  showErrorMsg :: String -> m a

instance CanFail (Either String) where
  showErrorMsg = throwError

instance CanFail [] where
  showErrorMsg _ = Ap.empty

instance CanFail Tree.Tree where
  showErrorMsg _ = Ap.empty

lookup :: (CanFail m, MonadTrans t, MonadState (Context a) (t m)) => Ident -> t m a
lookup x = do
  v <- use (at x)
  lift $ maybe (showErrorMsg $ "cannot find variable " <> show x) pure v

lookup' :: (CanFail m, MonadTrans t, MonadReader (Context a) (t m)) => Ident -> t m a
lookup' x = do
  v <- view (at x)
  lift $ maybe (showErrorMsg $ "cannot find variable " <> show x) pure v

put :: (CanFail m, MonadTrans t, MonadState (Context a) (t m)) => Ident -> a -> t m ()
put x v = do
  exists <- use (ctx . to (Map.member x))
  if exists
    then lift $ showErrorMsg ("variable " <> show x <> " already exists!")
    else at x ?= v

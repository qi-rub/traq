{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module QCompose.Data.Context (
  Context,

  -- * Lenses
  at,
  ins,
  ix,

  -- * Primary functions
  empty,
  null,
  (\\),
  trunc,
  merge,

  -- * Secondary functions
  fromList,
  singleton,
  toList,
  elems,

  -- * Monadic functions
  lookup,
  lookup',
  unsafePut,
  put,
) where

import Prelude hiding (lookup, null)
import qualified Prelude

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Text.Printf (printf)

import Lens.Micro hiding (at, ix)
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Control.MonadHelpers

type Ident = String

data Binding a = Binding Ident a
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

_binding :: Lens' (Binding a) (Ident, a)
_binding focus (Binding k v) = uncurry Binding <$> focus (k, v)

_var :: Lens' (Binding a) Ident
_var = _binding . _1

_val :: Lens' (Binding a) a
_val = _binding . _2

newtype Context a = Context [Binding a]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

_ctx :: Lens' (Context a) [Binding a]
_ctx focus (Context m) = Context <$> focus m

_binds :: Traversal' (Context a) (Binding a)
_binds focus (Context m) = Context <$> traverse focus m

-- Lenses

at :: Ident -> SimpleGetter (Context a) (Maybe a)
at k = _ctx . to (map $ view _binding) . to (Foldable.find ((== k) . fst)) . to (fmap snd)

ins :: Ident -> ASetter' (Context a) a
ins k = sets $ \f (Context bs) ->
  let v = f (error "Context.ins can only be used to set, not modify")
   in Context (Binding k v : bs)

-- | Get/Modify an existing binding.
ix :: Ident -> Traversal' (Context a) a
ix k focus = \(Context m) -> Context <$> go m
 where
  go [] = error $ "no binding: " <> k
  go (b : bs)
    | b ^. _var == k = (: bs) <$> _val focus b
    | otherwise = (b :) <$> go bs

-- Primary Functions

empty :: Context a
empty = Context []

null :: Context a -> Bool
null (Context m) = Prelude.null m

(\\) :: Context a -> Context a -> Context a
(Context m) \\ (Context m') = Context (List.deleteFirstsBy (\a b -> a ^. _var == b ^. _var) m m')

-- | truncate terms till `k`
trunc :: Ident -> Context a -> Context a
trunc k (Context m) = Context (go m)
 where
  go = tail . dropWhile ((k /=) . view _var)

merge :: Context a -> Context a -> Context a
merge (Context m) (Context m') = Context (m' ++ m)
-- * Conversions

fromList :: [(Ident, a)] -> Context a
fromList = Context . map (uncurry Binding) . reverse

toList :: Context a -> [(Ident, a)]
toList c = reverse $ c ^.. _binds . _binding

-- Secondary functions

singleton :: Ident -> a -> Context a
singleton k v = fromList [(k, v)]

elems :: Context a -> [a]
elems = map snd . toList

-- Monadic functions

lookup :: (MonadError String m, MonadState (Context a) m) => Ident -> m a
lookup x = use (at x) >>= maybeWithError (printf "cannot find variable `%s`" x)

lookup' :: (MonadError String m, MonadReader (Context a) m) => Ident -> m a
lookup' x = view (at x) >>= maybeWithError (printf "cannot find variable `%s`" x)

unsafePut :: (MonadState (Context a) m) => Ident -> a -> m ()
unsafePut x v = ins x .= v

put :: (MonadError String m, MonadState (Context a) m) => Ident -> a -> m ()
put x v =
  use (at x) >>= \case
    Nothing -> unsafePut x v
    _ -> throwError (printf "variable `%s` already exists!" x)

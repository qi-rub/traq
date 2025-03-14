{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module QCompose.Utils.Context (
  VarContext,
  SafeLookup (..),
) where

import Control.Monad.Except (throwError)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (find)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Basic

type VarContext a = M.Map Ident a

class (Monad m) => SafeLookup m where
  lookupVar :: (MonadTrans t, MonadState (VarContext a) (t m)) => Ident -> t m a
  putValue :: (MonadTrans t, MonadState (VarContext a) (t m)) => Ident -> a -> t m ()
  findBy :: (a -> Bool) -> [a] -> m a

instance SafeLookup (Either String) where
  lookupVar x = do
    v <- use (at x)
    lift $ maybe (throwError $ "cannot find variable " <> show x) pure v

  putValue x v = do
    exists <- gets (M.member x)
    if exists
      then lift $ throwError ("variable " <> show x <> " already exists!")
      else at x ?= v

  findBy predicate =
    maybe (throwError "no matching element") pure . find predicate

instance SafeLookup [] where
  lookupVar x = do
    v <- use (at x)
    lift $ maybe [] pure v

  putValue x v = do
    exists <- gets (M.member x)
    if exists
      then lift []
      else at x ?= v

  findBy predicate = maybe [] pure . find predicate

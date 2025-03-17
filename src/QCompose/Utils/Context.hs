{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module QCompose.Utils.Context (
  VarContext,
  CanFail (..),
  lookupVar,
  lookupVar',
  putValue,
  findBy,
) where

import Control.Applicative (empty)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (find)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Utils.Tree

import QCompose.Basic

type VarContext a = M.Map Ident a

class (Monad m) => CanFail m where
  showErrorMsg :: String -> m a

instance CanFail (Either String) where
  showErrorMsg = throwError

instance CanFail [] where
  showErrorMsg _ = empty

instance CanFail Tree where
  showErrorMsg _ = empty

lookupVar :: (CanFail m, MonadTrans t, MonadState (VarContext a) (t m)) => Ident -> t m a
lookupVar x = do
  v <- use (at x)
  lift $ maybe (showErrorMsg $ "cannot find variable " <> show x) pure v

lookupVar' :: (CanFail m, MonadTrans t, MonadReader (VarContext a) (t m)) => Ident -> t m a
lookupVar' x = do
  v <- view (at x)
  lift $ maybe (showErrorMsg $ "cannot find variable " <> show x) pure v

putValue :: (CanFail m, MonadTrans t, MonadState (VarContext a) (t m)) => Ident -> a -> t m ()
putValue x v = do
  exists <- gets (M.member x)
  if exists
    then lift $ showErrorMsg ("variable " <> show x <> " already exists!")
    else at x ?= v

findBy :: (CanFail m) => (a -> Bool) -> [a] -> m a
findBy predicate =
  maybe (showErrorMsg "no matching element") pure . find predicate

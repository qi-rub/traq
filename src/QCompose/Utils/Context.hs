{-# LANGUAGE FlexibleContexts #-}

module QCompose.Utils.Context (
  VarContext,
  lookupVar,
  putValue,
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, gets)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl
import QCompose.Basic

type VarContext a = M.Map Ident a

lookupVar :: (MonadError String m, MonadState (VarContext a) m) => Ident -> m a
lookupVar x = do
  v <- use (at x)
  maybe (throwError $ "cannot find variable " <> show x) pure v

putValue :: (MonadError String m, MonadState (VarContext a) m) => Ident -> a -> m ()
putValue x v = do
  exists <- gets (M.member x)
  if exists
    then throwError ("variable " <> show x <> " already exists!")
    else at x ?= v

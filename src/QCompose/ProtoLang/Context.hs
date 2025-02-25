module QCompose.ProtoLang.Context where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import QCompose.Basic

type Context k a = M.Map k a
type VarContext a = Context Ident a

lookupVar :: (Ord k, Show k) => k -> StateT (Context k a) (Either String) a
lookupVar x = do
  v <- gets (M.lookup x)
  lift $ maybe (throwError $ "cannot find variable " <> show x) Right v

putValue :: (Ord k, Show k) => k -> a -> StateT (Context k a) (Either String) ()
putValue x v = do
  exists <- gets (M.member x)
  if exists
    then throwError ("variable " <> show x <> " already exists!")
    else modify (M.insert x v)

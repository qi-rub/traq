{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module QCompose.ProtoLang.Syntax where

import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (find)
import QCompose.Basic
import QCompose.Utils.Rewriting

-- proto-search language
newtype VarType a = Fin a -- Fin<N>
  deriving (Eq, Show, Read, Functor)

data UnOp = NotOp
  deriving (Eq, Show, Read)

data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

data Stmt a
  = AssignS {ret :: Ident, arg :: Ident}
  | ConstS {ret :: Ident, val :: Value, ty :: VarType a}
  | UnOpS {ret :: Ident, un_op :: UnOp, arg :: Ident}
  | BinOpS {ret :: Ident, bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | OracleS {rets :: [Ident], args :: [Ident]}
  | FunCallS {rets :: [Ident], fun :: Ident, args :: [Ident]}
  | IfThenElseS {cond :: Ident, s_true :: Stmt a, s_false :: Stmt a}
  | SeqS [Stmt a]
  | SearchS {sol :: Ident, ok :: Ident, predicate :: Ident, args :: [Ident]}
  | ContainsS {ok :: Ident, predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read, Functor)

data FunDef a = FunDef
  { name :: Ident
  , params :: [(Ident, VarType a)]
  , rets :: [(Ident, VarType a)]
  , body :: Stmt a
  }
  deriving (Eq, Show, Read, Functor)

data OracleDef a = OracleDef
  { paramTypes :: [VarType a]
  , retTypes :: [VarType a]
  }
  deriving (Eq, Show, Read, Functor)

data FunCtx a = FunCtx
  { funDefs :: [FunDef a]
  , oracle :: OracleDef a
  }
  deriving (Eq, Show, Read, Functor)

data Program a = Program
  { funCtx :: FunCtx a
  , stmt :: Stmt a
  }
  deriving (Eq, Show, Read, Functor)

lookupFun :: (MonadError String m) => FunCtx a -> Ident -> m (FunDef a)
lookupFun FunCtx{..} fname =
  case find (\f -> name f == fname) funDefs of
    Nothing -> throwError $ "cannot find function " <> fname
    Just f -> return f

instance LocalRewritable (Stmt s) where
  rewrite rw (SeqS ss) = mapM rw ss >>= rw . SeqS
  rewrite rw s@IfThenElseS{..} = do
    s_true' <- rw s_true
    s_false' <- rw s_false
    rw $ s{s_true = s_true', s_false = s_false'}
  rewrite rw s = rw s

rewriteFunDef :: (Functor f) => (Stmt a -> f (Stmt a)) -> FunDef a -> f (FunDef a)
rewriteFunDef rw FunDef{..} =
  let fbody' = rw body
   in fmap (\body' -> FunDef{body = body', ..}) fbody'

rewriteFunCtx :: (Monad m) => (Stmt a -> m (Stmt a)) -> FunCtx a -> m (FunCtx a)
rewriteFunCtx rw funCtx@FunCtx{funDefs} = do
  funDefs' <- mapM (rewriteFunDef rw) funDefs
  return funCtx{funDefs = funDefs'}

rewriteProgram :: (Monad m) => (Stmt a -> m (Stmt a)) -> Program a -> m (Program a)
rewriteProgram rw p@Program{funCtx, stmt} = do
  funCtx' <- rewriteFunCtx rw funCtx
  stmt' <- rewrite rw stmt
  return p{funCtx = funCtx', stmt = stmt'}

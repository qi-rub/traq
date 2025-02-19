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

data UnOp = PNot
  deriving (Eq, Show, Read)

data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Stmt a
  = SAssign {ret :: Ident, arg :: Ident}
  | SConst {ret :: Ident, val :: Value, ty :: VarType a}
  | SUnOp {ret :: Ident, un_op :: UnOp, arg :: Ident}
  | SBinOp {ret :: Ident, bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | SOracle {rets :: [Ident], args :: [Ident]}
  | SFunCall {rets :: [Ident], fun :: Ident, args :: [Ident]}
  | SIfTE {cond :: Ident, s_true :: Stmt a, s_false :: Stmt a}
  | SSeq [Stmt a]
  | SSearch {sol :: Ident, ok :: Ident, predicate :: Ident, args :: [Ident]}
  | SContains {ok :: Ident, predicate :: Ident, args :: [Ident]}
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
  rewrite rw (SSeq ss) = mapM rw ss >>= rw . SSeq
  rewrite rw s@SIfTE{..} = do
    s_true' <- rw s_true
    s_false' <- rw s_false
    rw $ s{s_true = s_true', s_false = s_false'}
  rewrite rw s = rw s

rewriteFunDef :: (Monad m) => (Stmt a -> m (Stmt a)) -> FunDef a -> m (FunDef a)
rewriteFunDef rw funDef@FunDef{body} = do
  body' <- rw body
  return funDef{body = body'}

rewriteFunCtx :: (Monad m) => (Stmt a -> m (Stmt a)) -> FunCtx a -> m (FunCtx a)
rewriteFunCtx rw funCtx@FunCtx{funDefs} = do
  funDefs' <- mapM (rewriteFunDef rw) funDefs
  return funCtx{funDefs = funDefs'}

rewriteProgram :: (Monad m) => (Stmt a -> m (Stmt a)) -> Program a -> m (Program a)
rewriteProgram rw p@Program{funCtx, stmt} = do
  funCtx' <- rewriteFunCtx rw funCtx
  stmt' <- rewrite rw stmt
  return p{funCtx = funCtx', stmt = stmt'}

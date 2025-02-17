{-# LANGUAGE FlexibleContexts #-}

module QCompose.ProtoLang.Syntax where

import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (find)
import QCompose.Basic
import QCompose.Rewriting

-- proto-search language
newtype VarType = Fin (Symbolic SizeT) -- Fin<N>
  deriving (Eq, Show, Read)

data UnOp = PNot
  deriving (Eq, Show, Read)

data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Stmt
  = SAssign {ret :: Ident, arg :: Ident}
  | SConst {ret :: Ident, val :: Value, ty :: VarType}
  | SUnOp {ret :: Ident, un_op :: UnOp, arg :: Ident}
  | SBinOp {ret :: Ident, bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | SOracle {rets :: [Ident], args :: [Ident]}
  | SFunCall {rets :: [Ident], fun :: Ident, args :: [Ident]}
  | SIfTE {cond :: Ident, s_true :: Stmt, s_false :: Stmt}
  | SSeq [Stmt]
  | SSearch {sol :: Ident, ok :: Ident, predicate :: Ident, args :: [Ident]}
  | SContains {ok :: Ident, predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

data FunDef = FunDef
  { name :: Ident
  , params :: [(Ident, VarType)]
  , rets :: [(Ident, VarType)]
  , body :: Stmt
  }
  deriving (Eq, Show, Read)

data OracleDef = OracleDef
  { paramTypes :: [VarType]
  , retTypes :: [VarType]
  }
  deriving (Eq, Show, Read)

data FunCtx = FunCtx
  { funs :: [FunDef]
  , oracle :: OracleDef
  }
  deriving (Show)

data Program = Program
  { funCtx :: FunCtx
  , stmt :: Stmt
  }

lookupFun :: MonadError String m => FunCtx -> Ident -> m FunDef
lookupFun FunCtx{funs = fs} fname =
  case find (\f -> name f == fname) fs of
    Nothing -> throwError $ "cannot find function " <> fname
    Just f -> return f

instance LocalRewritable Stmt where
  rewrite rw (SSeq ss) = mapM rw ss >>= rw . SSeq
  rewrite rw s@SIfTE{..} = do
    s_true' <- rw s_true
    s_false' <- rw s_false
    rw $ s{s_true = s_true', s_false = s_false'}
  rewrite rw s = rw s

rewriteFunDef :: Monad m => (Stmt -> m Stmt) -> FunDef -> m FunDef
rewriteFunDef rw FunDef{..} = do
  body <- rw body
  return FunDef{..}

rewriteFunCtx :: Monad m => (Stmt -> m Stmt) -> FunCtx -> m FunCtx
rewriteFunCtx rw FunCtx{..} = do
  funs <- mapM (rewriteFunDef rw) funs
  return FunCtx{..}

rewriteProgram :: Monad m => (Stmt -> m Stmt) -> Program -> m Program
rewriteProgram rw Program{..} = do
  funCtx <- rewriteFunCtx rw funCtx
  stmt <- rewrite rw stmt
  return Program{..}

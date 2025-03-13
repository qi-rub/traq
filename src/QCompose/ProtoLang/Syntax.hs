{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module QCompose.ProtoLang.Syntax (
  VarType (..),
  UnOp (..),
  BinOp (..),
  Subroutine (..),
  FunctionCallKind (..),
  Stmt (..),
  FunDef (..),
  OracleDecl (..),
  Program (..),
  FunCtx (..),
  lookupFun,
  _ast,
  _stmt,
) where

import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (find)
import Lens.Micro (Traversal')
import QCompose.Basic
import QCompose.Utils.Printing

-- proto-search language
newtype VarType a = Fin a -- Fin<N>
  deriving (Eq, Show, Read, Functor)

data UnOp = NotOp
  deriving (Eq, Show, Read)

data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

-- | Built-in subroutines/primitives
data Subroutine
  = Search
  | Contains
  deriving (Eq, Show, Read, Enum, Bounded)

data FunctionCallKind
  = OracleCall
  | FunctionCall Ident
  | SubroutineCall Subroutine
  deriving (Eq, Show, Read)

-- | A statement in the prototype language.
data Stmt a
  = AssignS {ret :: Ident, arg :: Ident}
  | ConstS {ret :: Ident, val :: Value, ty :: VarType a}
  | UnOpS {ret :: Ident, un_op :: UnOp, arg :: Ident}
  | BinOpS {ret :: Ident, bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | IfThenElseS {cond :: Ident, s_true :: Stmt a, s_false :: Stmt a}
  | SeqS [Stmt a]
  | FunCallS {fun_kind :: FunctionCallKind, rets :: [Ident], args :: [Ident]}
  deriving (Eq, Show, Read, Functor)

-- | A function definition in the prototype language.
data FunDef a = FunDef
  { fun_name :: Ident
  , param_binds :: [(Ident, VarType a)]
  , ret_binds :: [(Ident, VarType a)]
  , body :: Stmt a
  }
  deriving (Eq, Show, Read, Functor)

-- | A declaration of the oracle's type
data OracleDecl a = OracleDecl
  { param_types :: [VarType a]
  , ret_types :: [VarType a]
  }
  deriving (Eq, Show, Read, Functor)

-- | A function context contains the oracle declaration, and a list of functions
data FunCtx a = FunCtx
  { fun_defs :: [FunDef a]
  , oracle :: OracleDecl a
  }
  deriving (Eq, Show, Read, Functor)

-- | A program is a function context with a statement (which acts like the `main`)
data Program a = Program
  { funCtx :: FunCtx a
  , stmt :: Stmt a
  }
  deriving (Eq, Show, Read, Functor)

lookupFun :: (MonadError String m) => FunCtx a -> Ident -> m (FunDef a)
lookupFun FunCtx{fun_defs} fname =
  case find ((fname ==) . fun_name) fun_defs of
    Nothing -> throwError $ "cannot find function " <> fname
    Just f -> return f

class HasAst a where
  _ast :: Traversal' a a

instance HasAst (Stmt a) where
  _ast f (SeqS ss) = SeqS <$> traverse f ss
  _ast f (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> f s_true <*> f s_false
  _ast _ s = pure s

class HasStmt p where
  _stmt :: Traversal' (p a) (Stmt a)

instance HasStmt FunDef where
  _stmt f funDef@FunDef{body} = (\body' -> funDef{body = body'}) <$> f body

instance HasStmt FunCtx where
  _stmt f (FunCtx fun_defs oracle) = FunCtx <$> traverse (_stmt f) fun_defs <*> pure oracle

instance HasStmt Program where
  _stmt f (Program funCtx stmt) = Program <$> _stmt f funCtx <*> f stmt

instance (Show a) => ToCodeString (VarType a) where
  toCodeString (Fin len) = "Fin<" <> show len <> ">"

instance ToCodeString UnOp where
  toCodeString NotOp = "!"

instance ToCodeString BinOp where
  toCodeString AddOp = "+"
  toCodeString LEqOp = "<="
  toCodeString AndOp = "/\\"

instance ToCodeString Subroutine where
  toCodeString Search = "search"
  toCodeString Contains = "any"

instance ToCodeString FunctionCallKind where
  toCodeString OracleCall = "Oracle"
  toCodeString (FunctionCall f) = f
  toCodeString (SubroutineCall f) = toCodeString f

instance (Show a) => ToCodeString (Stmt a) where
  toCodeLines AssignS{ret, arg} = [unwords [ret, "<-", arg]]
  toCodeLines ConstS{ret, val, ty} = [unwords [ret, "<-", show val, ":", toCodeString ty]]
  toCodeLines UnOpS{ret, un_op, arg} = [unwords [ret, "<-", toCodeString un_op <> arg]]
  toCodeLines BinOpS{ret, bin_op, lhs, rhs} = [unwords [ret, "<-", lhs, toCodeString bin_op, rhs]]
  toCodeLines FunCallS{fun_kind, rets, args} =
    [ unwords
        [ commaList rets
        , "<-"
        , toCodeString fun_kind <> "(" <> commaList args <> ")"
        ]
    ]
  toCodeLines IfThenElseS{cond, s_true, s_false} =
    [unwords ["if", cond, "then"]]
      <> indent (toCodeLines s_true)
      <> ["else"]
      <> indent (toCodeLines s_false)
      <> ["end"]
  toCodeLines (SeqS ss) = concatMap toCodeLines ss

instance (Show a) => ToCodeString (FunDef a) where
  toCodeLines FunDef{fun_name, param_binds, ret_binds, body} =
    [unwords ["def", fun_name, "(" <> commaList (showTypedVar <$> param_binds) <> ")", "do"]]
      <> indent
        ( toCodeLines body
            <> [unwords ["return", commaList (showTypedVar <$> ret_binds)]]
        )
      <> ["end"]
    where
      -- showTypedVar :: (Ident, VarType a) -> String
      showTypedVar (x, ty) = unwords [x, ":", toCodeString ty]

instance (Show a) => ToCodeString (OracleDecl a) where
  toCodeString OracleDecl{param_types, ret_types} =
    unwords
      [ "declare"
      , "Oracle" <> "(" <> commaList (toCodeString <$> param_types) <> ")"
      , "->"
      , commaList (toCodeString <$> ret_types)
      ]

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{funCtx = FunCtx{fun_defs, oracle}, stmt} =
    [toCodeString oracle, ""]
      <> fs
      <> toCodeLines stmt
    where
      fs = map toCodeString fun_defs

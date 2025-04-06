{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module QCompose.ProtoLang.Syntax (
  VarType (..),
  UnOp (..),
  BinOp (..),
  FunctionCallKind (..),
  Expr (..),
  Stmt (..),
  FunDef (..),
  OracleDecl (..),
  Program (..),
  FunCtx (..),
  mkFunDefCtx,
  _ast,
  _stmt,
) where

import Lens.Micro

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Utils.Printing

-- | Types
newtype VarType sizeT = Fin sizeT -- Fin<N>
  deriving (Eq, Show, Read, Functor)

-- | Unary operations
data UnOp = NotOp
  deriving (Eq, Show, Read)

-- | Binary operations
data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

data FunctionCallKind
  = OracleCall
  | FunctionCall Ident
  | PrimitiveCall Ident
  deriving (Eq, Show, Read)

{- | An expression in the prototype language.
 It appears as the RHS of an assignment statement.
-}
data Expr sizeT
  = VarE {arg :: Ident}
  | ConstE {val :: Value, ty :: VarType sizeT}
  | UnOpE {un_op :: UnOp, arg :: Ident}
  | BinOpE {bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | TernaryE {branch, lhs, rhs :: Ident}
  | FunCallE {fun_kind :: FunctionCallKind, args :: [Ident]}
  deriving (Eq, Show, Read, Functor)

-- | A statement in the prototype language.
data Stmt sizeT
  = ExprS {rets :: [Ident], expr :: Expr sizeT}
  | IfThenElseS {cond :: Ident, s_true :: Stmt sizeT, s_false :: Stmt sizeT}
  | SeqS [Stmt sizeT]
  deriving (Eq, Show, Read, Functor)

-- | A function definition in the prototype language.
data FunDef sizeT = FunDef
  { fun_name :: Ident
  , param_binds :: [(Ident, VarType sizeT)]
  , ret_binds :: [(Ident, VarType sizeT)]
  , body :: Stmt sizeT
  }
  deriving (Eq, Show, Read, Functor)

-- | A declaration of the oracle's type
data OracleDecl sizeT = OracleDecl
  { param_types :: [VarType sizeT]
  , ret_types :: [VarType sizeT]
  }
  deriving (Eq, Show, Read, Functor)

-- | A function context contains the oracle declaration, and a list of functions
data FunCtx sizeT = FunCtx
  { oracle_decl :: OracleDecl sizeT
  , fun_defs :: Ctx.Context (FunDef sizeT)
  }
  deriving (Eq, Show, Read, Functor)

-- | Create a mapping of function names to function defs.
mkFunDefCtx :: [FunDef a] -> Ctx.Context (FunDef a)
mkFunDefCtx fs = Ctx.fromList [(fun_name f, f) | f <- fs]

-- | A program is a function context with a statement (which acts like the `main`)
data Program sizeT = Program
  { funCtx :: FunCtx sizeT
  , stmt :: Stmt sizeT
  }
  deriving (Eq, Show, Read, Functor)

-- Lenses

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
  _stmt f (FunCtx oracle_decl fun_defs) = FunCtx oracle_decl <$> traverse (_stmt f) fun_defs

instance HasStmt Program where
  _stmt f (Program funCtx stmt) = Program <$> _stmt f funCtx <*> f stmt

-- Printing

instance (Show a) => ToCodeString (VarType a) where
  toCodeString (Fin len) = "Fin<" <> show len <> ">"

instance ToCodeString UnOp where
  toCodeString NotOp = "!"

instance ToCodeString BinOp where
  toCodeString AddOp = "+"
  toCodeString LEqOp = "<="
  toCodeString AndOp = "/\\"

instance ToCodeString FunctionCallKind where
  toCodeString OracleCall = "Oracle"
  toCodeString (FunctionCall f) = f
  toCodeString (PrimitiveCall f) = "@" ++ f

instance (Show a) => ToCodeString (Expr a) where
  toCodeString VarE{arg} = arg
  toCodeString ConstE{val, ty} = unwords [show val, ":", toCodeString ty]
  toCodeString UnOpE{un_op, arg} = toCodeString un_op <> arg
  toCodeString BinOpE{bin_op, lhs, rhs} =
    unwords [lhs, toCodeString bin_op, rhs]
  toCodeString TernaryE{branch, lhs, rhs} =
    unwords ["ifte", branch, lhs, rhs]
  toCodeString FunCallE{fun_kind, args} =
    unwords [toCodeString fun_kind <> "(" <> commaList args <> ")"]

instance (Show a) => ToCodeString (Stmt a) where
  toCodeLines ExprS{rets, expr} = [unwords [commaList rets, "<-", toCodeString expr]]
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
  toCodeLines Program{funCtx = FunCtx{oracle_decl, fun_defs}, stmt} =
    [toCodeString oracle_decl, ""]
      <> map toCodeString (Ctx.elems fun_defs)
      <> toCodeLines stmt

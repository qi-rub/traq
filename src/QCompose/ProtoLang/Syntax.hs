{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.ProtoLang.Syntax (
  -- * Syntax

  -- ** Basic Types and Operations
  VarType (..),
  UnOp (..),
  BinOp (..),

  -- ** Calls and Primitives
  FunctionCallKind (..),

  -- ** Expressions and Statements
  Expr (..),
  Stmt (..),
  FunBody (..),
  FunDef (..),
  FunCtx,
  Program (..),
) where

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Utils.ASTRewriting
import QCompose.Utils.Printing
import Text.Printf (printf)

-- ================================================================================
-- Syntax
-- ================================================================================

-- | Types
newtype VarType sizeT = Fin sizeT -- Fin<N>
  deriving (Eq, Show, Read, Functor)

-- | Unary operations
data UnOp = NotOp
  deriving (Eq, Show, Read)

-- | Binary operations
data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

-- | Either call an existing function, or an existing primitive.
data FunctionCallKind primT
  = FunctionCall Ident
  | PrimitiveCall {prim :: primT}
  deriving (Eq, Show, Read)

{- | An expression in the prototype language.
 It appears as the RHS of an assignment statement.
-}
data Expr primT sizeT
  = VarE {arg :: Ident}
  | ConstE {val :: Value, ty :: VarType sizeT}
  | UnOpE {un_op :: UnOp, arg :: Ident}
  | BinOpE {bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | TernaryE {branch, lhs, rhs :: Ident}
  | FunCallE {fun_kind :: FunctionCallKind primT, args :: [Ident]}
  deriving (Eq, Show, Read, Functor)

-- | A statement in the prototype language.
data Stmt primT sizeT
  = ExprS {rets :: [Ident], expr :: Expr primT sizeT}
  | IfThenElseS {cond :: Ident, s_true :: Stmt primT sizeT, s_false :: Stmt primT sizeT}
  | SeqS [Stmt primT sizeT]
  deriving (Eq, Show, Read, Functor)

-- | A function definition in the prototype language.
data FunBody primT sizeT = FunBody
  { param_names, ret_names :: [Ident]
  , body_stmt :: Stmt primT sizeT
  }
  deriving (Eq, Show, Read, Functor)

data FunDef primT sizeT = FunDef
  { fun_name :: Ident
  , param_types, ret_types :: [VarType sizeT]
  , mbody :: Maybe (FunBody primT sizeT)
  }
  deriving (Eq, Show, Read, Functor)

-- | A function context contains a list of functions
type FunCtx primT sizeT = Ctx.Context (FunDef primT sizeT)

-- | A program is a function context with a statement (which acts like the `main`)
data Program primT sizeT = Program
  { funCtx :: FunCtx primT sizeT
  , stmt :: Stmt primT sizeT
  }
  deriving (Eq, Show, Read, Functor)

-- ================================================================================
-- Lenses
-- ================================================================================

instance HasAst (Stmt primT sizeT) where
  _ast focus (SeqS ss) = SeqS <$> traverse focus ss
  _ast focus (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> focus s_true <*> focus s_false
  _ast _ s = pure s

instance HasStmt (Stmt primT sizeT) (Stmt primT sizeT) where
  _stmt = id

instance HasStmt (FunBody primT sizeT) (Stmt primT sizeT) where
  _stmt focus fbody@FunBody{body_stmt} = (\body_stmt' -> fbody{body_stmt = body_stmt'}) <$> focus body_stmt

instance HasStmt (FunDef primT sizeT) (Stmt primT sizeT) where
  _stmt focus funDef@FunDef{mbody = Just body} = (\body' -> funDef{mbody = Just body'}) <$> _stmt focus body
  _stmt _ f = pure f

instance HasStmt (Program primT sizeT) (Stmt primT sizeT) where
  _stmt focus (Program funCtx stmt) = Program <$> traverse (_stmt focus) funCtx <*> focus stmt

-- ================================================================================
-- Printing
-- ================================================================================

instance (Show a) => ToCodeString (VarType a) where
  toCodeString (Fin len) = "Fin<" <> show len <> ">"

instance ToCodeString UnOp where
  toCodeString NotOp = "!"

instance ToCodeString BinOp where
  toCodeString AddOp = "+"
  toCodeString LEqOp = "<="
  toCodeString AndOp = "/\\"

instance (ToCodeString primT) => ToCodeString (FunctionCallKind primT) where
  toCodeString (FunctionCall f) = f
  toCodeString (PrimitiveCall prim) = toCodeString prim

instance (Show sizeT, ToCodeString primT) => ToCodeString (Expr primT sizeT) where
  toCodeString VarE{arg} = arg
  toCodeString ConstE{val, ty} = unwords [show val, ":", toCodeString ty]
  toCodeString UnOpE{un_op, arg} = toCodeString un_op <> arg
  toCodeString BinOpE{bin_op, lhs, rhs} =
    unwords [lhs, toCodeString bin_op, rhs]
  toCodeString TernaryE{branch, lhs, rhs} =
    unwords ["ifte", branch, lhs, rhs]
  toCodeString FunCallE{fun_kind, args} =
    unwords [toCodeString fun_kind <> "(" <> commaList args <> ")"]

instance (Show sizeT, ToCodeString primT) => ToCodeString (Stmt primT sizeT) where
  toCodeLines ExprS{rets, expr} = [unwords [commaList rets, "<-", toCodeString expr]]
  toCodeLines IfThenElseS{cond, s_true, s_false} =
    [unwords ["if", cond, "then"]]
      <> indent (toCodeLines s_true)
      <> ["else"]
      <> indent (toCodeLines s_false)
      <> ["end"]
  toCodeLines (SeqS ss) = concatMap toCodeLines ss

instance (Show sizeT, ToCodeString primT) => ToCodeString (FunDef primT sizeT) where
  -- def
  toCodeLines
    FunDef
      { fun_name
      , param_types
      , ret_types
      , mbody = Just FunBody{body_stmt, param_names, ret_names}
      } =
      [printf "def %s(%s) do" fun_name (commaList $ zipWith showTypedVar param_names param_types)]
        <> indent
          ( toCodeLines body_stmt
              <> [unwords ["return", commaList $ zipWith showTypedVar ret_names ret_types]]
          )
        <> ["end"]
     where
      showTypedVar :: Ident -> VarType sizeT -> String
      showTypedVar x ty = unwords [x, ":", toCodeString ty]
  -- declare
  toCodeLines FunDef{fun_name, param_types, ret_types, mbody = Nothing} =
    [ printf
        "declare %s(%s) -> %s"
        fun_name
        (commaList $ map toCodeString param_types)
        (commaList $ map toCodeString ret_types)
    ]

instance (Show sizeT, ToCodeString primT) => ToCodeString (Program primT sizeT) where
  toCodeLines Program{funCtx, stmt} =
    map toCodeString (Ctx.elems funCtx)
      <> toCodeLines stmt

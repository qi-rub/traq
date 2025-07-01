{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.ProtoLang.Syntax (
  -- * Syntax

  -- ** Basic Types and Operations
  VarType (..),
  UnOp (..),
  BinOp (..),
  BasicExpr (..),

  -- ** Calls and Primitives
  FunctionCallKind (..),

  -- ** Expressions and Statements
  Expr (..),
  Stmt (..),
  FunBody (..),
  FunDef (..),
  NamedFunDef (..),
  FunCtx,
  Program (..),

  -- ** Lenses
  HasFunCtx (..),
) where

import Lens.Micro.GHC
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang.Prelude
import QCompose.Utils.ASTRewriting
import QCompose.Utils.Printing

-- ================================================================================
-- Common Syntax
-- ================================================================================

-- | Types
newtype VarType sizeT = Fin sizeT -- Fin<N>
  deriving (Eq, Show, Read, Functor)

type instance SizeType (VarType sizeT) = sizeT

instance (Show a) => ToCodeString (VarType a) where
  toCodeString (Fin len) = "Fin<" <> show len <> ">"

-- | Unary operations
data UnOp = NotOp
  deriving (Eq, Show, Read)

instance ToCodeString UnOp where
  toCodeString NotOp = "not "

-- | Binary operations
data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

instance ToCodeString BinOp where
  toCodeString AddOp = "+"
  toCodeString LEqOp = "<="
  toCodeString AndOp = "&&"

-- | Operations which take multiple arguments
data NAryOp = MultiOrOp
  deriving (Eq, Show, Read)

instance ToCodeString NAryOp where
  toCodeString MultiOrOp = "or"

-- | Basic arithmetic and logical expressions
data BasicExpr sizeT
  = VarE {var :: Ident}
  | ParamE {param :: Ident} -- compile-time constant parameter
  | ConstE {val :: Value, ty :: VarType sizeT}
  | UnOpE {un_op :: UnOp, operand :: BasicExpr sizeT}
  | BinOpE {bin_op :: BinOp, lhs, rhs :: BasicExpr sizeT}
  | TernaryE {branch, lhs, rhs :: BasicExpr sizeT}
  | NAryE {op :: NAryOp, operands :: [BasicExpr sizeT]}
  deriving (Eq, Show, Read, Functor)

instance (Show sizeT) => ToCodeString (BasicExpr sizeT) where
  toCodeString VarE{var} = var
  toCodeString ParamE{param} = printf "#%s" param
  toCodeString ConstE{val, ty} = unwords [show val, ":", toCodeString ty]
  toCodeString UnOpE{un_op, operand} = toCodeString un_op <> toCodeString operand
  toCodeString BinOpE{bin_op, lhs, rhs} =
    "(" <> unwords [toCodeString lhs, toCodeString bin_op, toCodeString rhs] <> ")"
  toCodeString TernaryE{branch, lhs, rhs} =
    "(" <> unwords ("ifte" : map toCodeString [branch, lhs, rhs]) <> ")"
  toCodeString NAryE{op, operands} = toCodeString op <> "(" <> commaList (map toCodeString operands) <> ")"

-- -- | Expressions (RHS of an assignment operation)
-- data Expr sizeT
--   = ConstE {val :: Value, val_ty :: VarType sizeT}
--   | MetaValE {meta_val :: MetaParam sizeT, val_ty :: VarType sizeT}
--   | VarE {var :: Ident}
--   | AddE {lhs, rhs :: Expr sizeT}
--   | MulE {lhs, rhs :: Expr sizeT}
--   | LEqE {lhs, rhs :: Expr sizeT}
--   | AndE {lhs, rhs :: Expr sizeT}
--   | NotE {arg :: Expr sizeT}
--   | MinE {lhs, rhs :: Expr sizeT}
--   deriving (Eq, Show, Read)
-- instance (Show sizeT) => ToCodeString (P.BasicExpr sizeT) where
--   toCodeString ConstE{val, val_ty} = show val <> " : " <> toCodeString val_ty
--   toCodeString MetaValE{meta_val} = toCodeString meta_val
--   toCodeString VarE{var} = var
--   toCodeString AddE{lhs, rhs} = parenBinExpr "+" lhs rhs
--   toCodeString MulE{lhs, rhs} = parenBinExpr "*" lhs rhs
--   toCodeString LEqE{lhs, rhs} = parenBinExpr "<=" lhs rhs
--   toCodeString AndE{lhs, rhs} = parenBinExpr "&&" lhs rhs
--   toCodeString NotE{arg} = "!" ++ toCodeString arg
--   toCodeString MinE{lhs, rhs} =
--     "min(" ++ toCodeString lhs ++ ", " ++ toCodeString rhs ++ ")"

-- parenBinExpr :: (Show sizeT) => String -> P.BasicExpr sizeT -> P.BasicExpr sizeT -> String
-- parenBinExpr op_sym lhs rhs =
--   "(" ++ unwords [toCodeString lhs, op_sym, toCodeString rhs] ++ ")"

-- ================================================================================
-- Syntax
-- ================================================================================

-- | Either call an existing function, or an existing primitive.
data FunctionCallKind primT
  = FunctionCall Ident
  | PrimitiveCall {prim :: primT}
  deriving (Eq, Show, Read)

type instance PrimitiveType (FunctionCallKind primT) = primT

{- | An expression in the prototype language.
 It appears as the RHS of an assignment statement.
-}
data Expr primT sizeT
  = BasicExprE {basic_expr :: BasicExpr sizeT}
  | FunCallE {fun_kind :: FunctionCallKind primT, args :: [Ident]}
  deriving (Eq, Show, Read, Functor)

type instance SizeType (Expr primT sizeT) = sizeT
type instance PrimitiveType (Expr primT sizeT) = primT

-- | A statement in the prototype language.
data Stmt primT sizeT
  = ExprS {rets :: [Ident], expr :: Expr primT sizeT}
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt primT sizeT}
  | SeqS [Stmt primT sizeT]
  deriving (Eq, Show, Read, Functor)

type instance SizeType (Stmt primT sizeT) = sizeT
type instance PrimitiveType (Stmt primT sizeT) = primT

-- | The body of a function.
data FunBody primT sizeT = FunBody
  { param_names, ret_names :: [Ident]
  , body_stmt :: Stmt primT sizeT
  }
  deriving (Eq, Show, Read, Functor)

type instance SizeType (FunBody primT sizeT) = sizeT
type instance PrimitiveType (FunBody primT sizeT) = primT

-- | A function definition or declaration in the prototype language.
data FunDef primT sizeT = FunDef
  { param_types, ret_types :: [VarType sizeT]
  , mbody :: Maybe (FunBody primT sizeT)
  }
  deriving (Eq, Show, Read, Functor)

type instance SizeType (FunDef primT sizeT) = sizeT
type instance PrimitiveType (FunDef primT sizeT) = primT

-- | A function with a name
data NamedFunDef primT sizeT = NamedFunDef {fun_name :: Ident, fun_def :: FunDef primT sizeT}
  deriving (Eq, Show, Read, Functor)

type instance SizeType (NamedFunDef primT sizeT) = sizeT
type instance PrimitiveType (NamedFunDef primT sizeT) = primT

-- | A function context contains a list of functions
type FunCtx primT sizeT = Ctx.Context (FunDef primT sizeT)

type instance SizeType (FunCtx primT sizeT) = sizeT
type instance PrimitiveType (FunCtx primT sizeT) = primT

class HasFunCtx p where
  _funCtx :: (primT ~ PrimitiveType p, sizeT ~ SizeType p) => Lens' p (FunCtx primT sizeT)

instance HasFunCtx (FunCtx primT sizeT) where _funCtx = id

-- | A program is a function context with a statement (which acts like the `main`)
data Program primT sizeT = Program
  { funCtx :: FunCtx primT sizeT
  , stmt :: Stmt primT sizeT
  }
  deriving (Eq, Show, Read, Functor)

type instance SizeType (Program primT sizeT) = sizeT
type instance PrimitiveType (Program primT sizeT) = primT

instance HasFunCtx (Program primT sizeT) where
  _funCtx focus p@Program{funCtx} = focus funCtx <&> \funCtx' -> p{funCtx = funCtx'}

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

instance (ToCodeString primT) => ToCodeString (FunctionCallKind primT) where
  toCodeString (FunctionCall f) = f
  toCodeString (PrimitiveCall prim) = toCodeString prim

instance (Show sizeT, ToCodeString primT) => ToCodeString (Expr primT sizeT) where
  toCodeString BasicExprE{basic_expr} = toCodeString basic_expr
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

instance (Show sizeT, ToCodeString primT) => ToCodeString (NamedFunDef primT sizeT) where
  -- def
  toCodeLines
    NamedFunDef
      { fun_name
      , fun_def =
        FunDef
          { param_types
          , ret_types
          , mbody = Just FunBody{body_stmt, param_names, ret_names}
          }
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
  toCodeLines
    NamedFunDef
      { fun_name
      , fun_def = FunDef{param_types, ret_types, mbody = Nothing}
      } =
      [ printf
          "declare %s(%s) -> %s;"
          fun_name
          (commaList $ map toCodeString param_types)
          (commaList $ map toCodeString ret_types)
      ]

instance (Show sizeT, ToCodeString primT) => ToCodeString (Program primT sizeT) where
  toCodeLines Program{funCtx, stmt} =
    [toCodeString NamedFunDef{fun_name, fun_def} | (fun_name, fun_def) <- Ctx.toList funCtx]
      <> toCodeLines stmt

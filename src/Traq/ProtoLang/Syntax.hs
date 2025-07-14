{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.ProtoLang.Syntax (
  -- * Syntax
  MetaParam (..),

  -- ** Basic Types and Operations
  VarType (..),
  UnOp (..),
  BinOp (..),
  NAryOp (..),
  BasicExpr (..),
  (.<=.),
  notE,
  (.+.),
  (.&&.),

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

import Data.String (IsString (..))
import Lens.Micro.GHC
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.Utils.ASTRewriting
import qualified Traq.Utils.Printing as PP

-- | Compile-time constant parameters
data MetaParam sizeT = MetaName String | MetaSize sizeT | MetaValue Value
  deriving (Eq, Show, Read)

instance (Show sizeT) => PP.ToCodeString (MetaParam sizeT) where
  build (MetaName n) = PP.putWord $ "#" ++ n
  build (MetaSize n) = PP.putWord $ show n
  build (MetaValue n) = PP.putWord $ show n

-- ================================================================================
-- Common Syntax
-- ================================================================================

-- | Types
newtype VarType sizeT = Fin sizeT -- Fin<N>
  deriving (Eq, Show, Read, Functor)

type instance SizeType (VarType sizeT) = sizeT

instance (Show a) => PP.ToCodeString (VarType a) where
  build (Fin len) = PP.putWord $ "Fin<" <> show len <> ">"

-- | Unary operations
data UnOp = NotOp
  deriving (Eq, Show, Read)

instance PP.ToCodeString UnOp where
  build NotOp = PP.putWord "not "

-- | Binary operations
data BinOp = AddOp | LEqOp | AndOp
  deriving (Eq, Show, Read)

instance PP.ToCodeString BinOp where
  build AddOp = PP.putWord "+"
  build LEqOp = PP.putWord "<="
  build AndOp = PP.putWord "&&"

-- | Operations which take multiple arguments
data NAryOp = MultiOrOp
  deriving (Eq, Show, Read)

instance PP.ToCodeString NAryOp where
  build MultiOrOp = PP.putWord "or"

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

-- Helpers for shorter expressions
instance IsString (BasicExpr sizeT) where
  fromString ('#' : s) = ParamE s
  fromString s = VarE s

notE :: BasicExpr sizeT -> BasicExpr sizeT
notE = UnOpE NotOp

(.<=.), (.+.), (.&&.) :: BasicExpr sizeT -> BasicExpr sizeT -> BasicExpr sizeT
(.<=.) = BinOpE LEqOp
(.+.) = BinOpE AddOp
(.&&.) = BinOpE AndOp

instance (Show sizeT) => PP.ToCodeString (BasicExpr sizeT) where
  build VarE{var} = PP.putWord var
  build ParamE{param} = PP.putWord $ printf "#%s" param
  build ConstE{val, ty} = PP.putWord . printf "%s:%s" (show val) =<< PP.fromBuild ty
  build UnOpE{un_op, operand} =
    PP.putWord =<< (++) <$> PP.fromBuild un_op <*> PP.fromBuild operand
  build BinOpE{bin_op, lhs, rhs} =
    PP.putWord =<< printf "(%s %s %s)" <$> PP.fromBuild lhs <*> PP.fromBuild bin_op <*> PP.fromBuild rhs
  build TernaryE{branch, lhs, rhs} =
    PP.putWord =<< printf "(ifte %s %s %s)" <$> PP.fromBuild branch <*> PP.fromBuild lhs <*> PP.fromBuild rhs
  build NAryE{op, operands} =
    PP.putWord
      =<< printf "%s(%s)"
        <$> PP.fromBuild op
        <*> (PP.commaList <$> mapM PP.fromBuild operands)

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

instance (PP.ToCodeString primT) => PP.ToCodeString (FunctionCallKind primT) where
  build (FunctionCall f) = PP.putWord f
  build (PrimitiveCall prim) = PP.putWord $ PP.toCodeString prim

instance (Show sizeT, PP.ToCodeString primT) => PP.ToCodeString (Expr primT sizeT) where
  build BasicExprE{basic_expr} = PP.build basic_expr
  build FunCallE{fun_kind, args} = do
    fun_kind_s <- PP.fromBuild fun_kind
    PP.putLine $ printf "%s(%s)" fun_kind_s (PP.commaList args)

instance (Show sizeT, PP.ToCodeString primT) => PP.ToCodeString (Stmt primT sizeT) where
  build ExprS{rets, expr} = do
    expr_s <- PP.fromBuild expr
    PP.putLine $ unwords [PP.commaList rets, "<-", expr_s]
  build IfThenElseS{cond, s_true, s_false} = do
    PP.putLine $ printf "if (%s) then" cond
    PP.indented $ PP.build s_true
    PP.putLine "else"
    PP.indented $ do PP.build s_false
    PP.putLine "end"
  build (SeqS ss) = mapM_ PP.build ss

instance (Show sizeT, PP.ToCodeString primT) => PP.ToCodeString (NamedFunDef primT sizeT) where
  -- def
  build
    NamedFunDef
      { fun_name
      , fun_def =
        FunDef
          { param_types
          , ret_types
          , mbody = Just FunBody{body_stmt, param_names, ret_names}
          }
      } = do
      PP.putLine $ printf "def %s(%s) do" fun_name (PP.commaList $ zipWith showTypedVar param_names param_types)
      PP.indented $ do
        PP.build body_stmt
        PP.putLine $ unwords ["return", PP.commaList $ zipWith showTypedVar ret_names ret_types]
      PP.putLine "end"
     where
      showTypedVar :: Ident -> VarType sizeT -> String
      showTypedVar x ty = unwords [x, ":", PP.toCodeString ty]
  -- declare
  build
    NamedFunDef
      { fun_name
      , fun_def = FunDef{param_types, ret_types, mbody = Nothing}
      } =
      PP.putLine $
        printf
          "declare %s(%s) -> (%s) end"
          fun_name
          (PP.commaList $ map PP.toCodeString param_types)
          (PP.commaList $ map PP.toCodeString ret_types)

instance (Show sizeT, PP.ToCodeString primT) => PP.ToCodeString (Program primT sizeT) where
  build Program{funCtx, stmt} = do
    sequence_
      [ PP.build NamedFunDef{fun_name, fun_def} >> PP.endl
      | (fun_name, fun_def) <- Ctx.toList funCtx
      ]
    PP.build stmt

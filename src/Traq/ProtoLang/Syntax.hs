{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.ProtoLang.Syntax (
  -- * Syntax
  MetaParam (..),

  -- ** Basic Types
  VarType (..),
  _Fin,
  _Arr,
  _Tup,
  Value (..),
  _FinV,
  _ArrV,
  _TupV,

  -- ** Basic Operations
  UnOp (..),
  BinOp (..),
  NAryOp (..),
  BasicExpr (..),
  (.<=.),
  notE,
  (.+.),
  (.&&.),

  -- ** Expressions and Statements
  DistrExpr (..),
  Expr (..),
  Stmt (..),
  FunBody (..),
  FunDef (..),
  NamedFunDef (..),
  FunCtx,
  namedFunsToFunCtx,
  Program (..),

  -- ** Core
  Core,

  -- ** Core Language with default types.
  Core',

  -- ** Lenses
  HasFunCtx (..),
) where

import Control.Monad (zipWithM)
import Data.Foldable (toList)
import Data.String (IsString (..))
import Text.Printf (printf)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.Utils.Printing as PP

-- | Compile-time constant parameters
data MetaParam sizeT = MetaName String | MetaSize sizeT | MetaValue Integer
  deriving (Eq, Show, Read)

instance (Show sizeT) => PP.ToCodeString (MetaParam sizeT) where
  build (MetaName n) = PP.putWord $ "#" ++ n
  build (MetaSize n) = PP.putWord $ show n
  build (MetaValue n) = PP.putWord $ show n

-- ================================================================================
-- Types and Values
-- ================================================================================

-- | Types
data VarType sizeT
  = Fin sizeT -- Fin<N>
  | Arr sizeT (VarType sizeT) -- Arr<N, T>
  | Tup [VarType sizeT] -- (T_1, ..., T_k)
  deriving (Eq, Show, Read, Functor)

type instance SizeType (VarType sizeT) = sizeT

_Fin :: Traversal' (VarType sizeT) sizeT
_Fin focus (Fin n) = Fin <$> focus n
_Fin _ t = pure t

_Arr :: Traversal' (VarType sizeT) (sizeT, VarType sizeT)
_Arr focus (Arr n t) = uncurry Arr <$> focus (n, t)
_Arr _ t = pure t

_Tup :: Traversal' (VarType sizeT) [VarType sizeT]
_Tup focus (Tup ts) = Tup <$> focus ts
_Tup _ t = pure t

instance (Show a) => PP.ToCodeString (VarType a) where
  build (Fin len) = PP.putWord $ "Fin<" <> show len <> ">"
  build (Arr n t) = PP.putWord . printf "Arr<%s, %s>" (show n) =<< PP.fromBuild t
  build (Tup ts) = do
    ws <- mapM PP.fromBuild ts
    PP.putWord $ printf "Tup<%s>" $ PP.commaList ws

-- | Basic Values
data Value sizeT
  = -- | value of type @Fin n@
    FinV sizeT
  | -- | value of type @Arr n t@
    ArrV [Value sizeT]
  | -- | tuple value
    TupV [Value sizeT]
  deriving (Eq, Ord, Show, Read, Functor)

type instance SizeType (Value sizeT) = sizeT

_FinV :: Traversal' (Value sizeT) sizeT
_FinV focus (FinV n) = FinV <$> focus n
_FinV _ t = pure t

_ArrV :: Traversal' (Value sizeT) [Value sizeT]
_ArrV focus (ArrV vs) = ArrV <$> focus vs
_ArrV _ t = pure t

_TupV :: Traversal' (Value sizeT) [Value sizeT]
_TupV focus (TupV vs) = TupV <$> focus vs
_TupV _ t = pure t

instance (Show sizeT) => PP.ToCodeString (Value sizeT) where
  build (FinV v) = PP.putWord $ show v
  build (ArrV vs) = PP.joined (printf "[%s]" . PP.commaList) $ mapM_ PP.build vs
  build (TupV vs) = PP.joined (printf "(%s)" . PP.commaList) $ mapM_ PP.build vs

-- ================================================================================
-- Basic Expressions
-- ================================================================================

-- | Unary operations
data UnOp = NotOp
  deriving (Eq, Show, Read)

instance PP.ToCodeString UnOp where
  build NotOp = PP.putWord "not "

-- | Binary operations
data BinOp
  = AddOp
  | MulOp
  | SubOp
  | XorOp
  | LEqOp
  | LtOp
  | AndOp
  deriving (Eq, Show, Read)

instance PP.ToCodeString BinOp where
  build AddOp = PP.putWord "+"
  build MulOp = PP.putWord "*"
  build SubOp = PP.putWord "-"
  build XorOp = PP.putWord "^"
  build LEqOp = PP.putWord "<="
  build LtOp = PP.putWord "<"
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
  | DefaultE {ty :: VarType sizeT} -- initialize to default value of `ty`
  | ConstE {val :: Value sizeT, ty :: VarType sizeT}
  | UnOpE {un_op :: UnOp, operand :: BasicExpr sizeT}
  | BinOpE {bin_op :: BinOp, lhs, rhs :: BasicExpr sizeT}
  | TernaryE {branch, lhs, rhs :: BasicExpr sizeT}
  | NAryE {op :: NAryOp, operands :: [BasicExpr sizeT]}
  | -- array
    IndexE {arr_expr :: BasicExpr sizeT, ix_val :: sizeT}
  | DynIndexE {arr_expr, ix_expr :: BasicExpr sizeT}
  | UpdateArrE {arr_expr, ix_expr, rhs :: BasicExpr sizeT}
  | -- tuple
    ProjectE {tup_expr :: BasicExpr sizeT, tup_ix_val :: Int}
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
  build DefaultE{ty} = PP.putWord . printf "default : %s" =<< PP.fromBuild ty
  build ConstE{val, ty} =
    PP.putWord =<< printf "%s:%s" <$> PP.fromBuild val <*> PP.fromBuild ty
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
  build IndexE{arr_expr, ix_val} =
    PP.putWord =<< printf "%s[%s]" <$> PP.fromBuild arr_expr <*> pure (show ix_val)
  build DynIndexE{arr_expr, ix_expr} =
    PP.putWord =<< printf "%s[%s]" <$> PP.fromBuild arr_expr <*> PP.fromBuild ix_expr
  build UpdateArrE{arr_expr, ix_expr, rhs} =
    PP.putWord =<< printf "update %s[%s] = %s" <$> PP.fromBuild arr_expr <*> PP.fromBuild ix_expr <*> PP.fromBuild rhs
  build ProjectE{tup_expr, tup_ix_val} =
    PP.putWord =<< printf "%s.%d" <$> PP.fromBuild tup_expr <*> pure tup_ix_val

-- ================================================================================
-- Syntax
-- ================================================================================

-- | An expression denoting a probablity distribution.
data DistrExpr sizeT
  = UniformE {sample_ty :: VarType sizeT}
  | BernoulliE {prob_one :: Double}
  deriving (Eq, Show, Read, Functor)

type instance SizeType (DistrExpr sizeT) = sizeT

instance (Show sizeT) => PP.ToCodeString (DistrExpr sizeT) where
  build UniformE{sample_ty} = PP.putWord . printf "uniform : %s" =<< PP.fromBuild sample_ty
  build BernoulliE{prob_one} = PP.putWord $ printf "bernoulli[%s]" (show prob_one)

{- | An expression in the prototype language.
 It appears as the RHS of an assignment statement.
-}
data Expr ext
  = BasicExprE {basic_expr :: BasicExpr (SizeType ext)}
  | RandomSampleE {distr_expr :: DistrExpr (SizeType ext)}
  | FunCallE {fname :: Ident, args :: [Ident]}
  | PrimCallE {prim :: ext}
  | LoopE {initial_args :: [Ident], loop_body_fun :: Ident}

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (Expr ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (Expr ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (Expr ext)

type instance SizeType (Expr ext) = SizeType ext
type instance PrecType (Expr ext) = PrecType ext
type instance ExtensionType (Expr ext) = ext

instance (Show (SizeType ext), PP.ToCodeString ext) => PP.ToCodeString (Expr ext) where
  build BasicExprE{basic_expr} = PP.build basic_expr
  build RandomSampleE{distr_expr} = PP.putLine . printf "$ %s" =<< PP.fromBuild distr_expr
  build FunCallE{fname, args} = PP.putLine $ printf "%s(%s)" fname (PP.commaList args)
  build PrimCallE{prim} = PP.build prim
  build LoopE{initial_args, loop_body_fun} = do
    let args = PP.commaList initial_args
    PP.putWord $ printf "loop (%s) %s" args loop_body_fun

-- | A statement in the prototype language.
data Stmt ext
  = ExprS {rets :: [Ident], expr :: Expr ext}
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt ext}
  | SeqS [Stmt ext]

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (Stmt ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (Stmt ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (Stmt ext)

type instance SizeType (Stmt ext) = SizeType ext
type instance PrecType (Stmt ext) = PrecType ext
type instance ExtensionType (Stmt ext) = ext

instance (Show (SizeType ext), PP.ToCodeString ext) => PP.ToCodeString (Stmt ext) where
  build ExprS{rets, expr} = do
    expr_s <- PP.fromBuild expr
    PP.putLine $ case expr_s of
      ('$' : ' ' : expr_s') -> printf "%s <-$ %s;" (PP.commaList rets) expr_s'
      _ -> printf "%s <- %s;" (PP.commaList rets) expr_s
  build IfThenElseS{cond, s_true, s_false} = do
    PP.putLine $ printf "if (%s) then" cond
    PP.indented $ PP.build s_true
    PP.putLine "else"
    PP.indented $ PP.build s_false
    PP.putLine "end"
  build (SeqS ss) = mapM_ PP.build ss

-- | The body of a function.
data FunBody ext = FunBody
  { param_names, ret_names :: [Ident]
  , body_stmt :: Stmt ext
  }

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (FunBody ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (FunBody ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (FunBody ext)

type instance SizeType (FunBody ext) = SizeType ext
type instance PrecType (FunBody ext) = PrecType ext
type instance ExtensionType (FunBody ext) = ext

-- | A function definition or declaration in the prototype language.
data FunDef ext = FunDef
  { param_types, ret_types :: [VarType (SizeType ext)]
  , mbody :: Maybe (FunBody ext)
  }

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (FunDef ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (FunDef ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (FunDef ext)

type instance SizeType (FunDef ext) = SizeType ext
type instance PrecType (FunDef ext) = PrecType ext
type instance ExtensionType (FunDef ext) = ext

-- | A function with a name
data NamedFunDef ext = NamedFunDef {fun_name :: Ident, fun_def :: FunDef ext}

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (NamedFunDef ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (NamedFunDef ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (NamedFunDef ext)

type instance SizeType (NamedFunDef ext) = SizeType ext
type instance PrecType (NamedFunDef ext) = PrecType ext
type instance ExtensionType (NamedFunDef ext) = ext

instance (Show (SizeType ext), PP.ToCodeString ext) => PP.ToCodeString (NamedFunDef ext) where
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
      params <- PP.commaList <$> zipWithM showTypedVar param_names param_types
      s_ret_tys <- case ret_types of
        [t] -> PP.fromBuild t
        _ -> PP.commaList <$> mapM PP.fromBuild ret_types
      PP.putLine $ printf "def %s(%s) -> %s do" fun_name params s_ret_tys
      PP.indented $ do
        PP.build body_stmt
        PP.putLine $ printf "return %s" (PP.commaList ret_names)
      PP.putLine "end"
     where
      -- showTypedVar :: Ident -> VarType sizeT -> String
      showTypedVar x ty = printf "%s: %s" x <$> PP.fromBuild ty
  -- declare
  build
    NamedFunDef
      { fun_name
      , fun_def = FunDef{param_types, ret_types, mbody = Nothing}
      } =
      PP.putLine
        =<< printf "declare %s(%s) -> (%s) end" fun_name
          <$> (PP.commaList <$> mapM PP.fromBuild param_types)
          <*> (PP.commaList <$> mapM PP.fromBuild ret_types)

-- | A function context contains a list of functions
type FunCtx ext = Ctx.Context (FunDef ext)

class HasFunCtx p ext | p -> ext where
  _funCtx :: Lens' p (FunCtx ext)

instance HasFunCtx (FunCtx ext) ext where _funCtx = id

-- | A program is a list of named functions, with the last being the entry point.
newtype Program ext = Program [NamedFunDef ext]

deriving instance (Eq ext, Eq (SizeType ext)) => Eq (Program ext)
deriving instance (Show ext, Show (SizeType ext)) => Show (Program ext)
deriving instance (Read ext, Read (SizeType ext)) => Read (Program ext)

type instance SizeType (Program ext) = SizeType ext
type instance PrecType (Program ext) = PrecType ext
type instance ExtensionType (Program ext) = PrecType ext

namedFunsToFunCtx :: (Foldable f) => f (NamedFunDef ext) -> FunCtx ext
namedFunsToFunCtx fs = Ctx.fromList [(fun_name f, fun_def f) | f <- toList fs]

instance (Show (SizeType ext), PP.ToCodeString ext) => PP.ToCodeString (Program ext) where
  build (Program fs) = mapM_ (\f -> PP.build f >> PP.endl) fs

{- | Void extension (i.e. only use the core language)
Usage: @p :: Program (Core sizeT precT)@
-}
data Core sizeT precT
  deriving (Eq, Read, Show)

type instance SizeType (Core size prec) = size
type instance PrecType (Core size prec) = prec

{- | Simple void extension with integer size and double prec.
Usage: @p :: Program Core'@
-}
type Core' = Core SizeT Double

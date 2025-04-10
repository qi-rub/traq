{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.CQPL.Syntax (
  -- * Syntax
  MetaParam (..),
  Expr (..),
  FunctionCall (..),
  Stmt (..),
  ProcDef (..),
  OracleDecl (..),
  Program (..),

  -- * Syntax Sugar
  desugarS,
) where

import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang (VarType)
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.ASTRewriting
import QCompose.Utils.Printing

-- ================================================================================
-- Syntax
-- ================================================================================

-- | Compile-time constant parameters
data MetaParam sizeT = NameMeta String | SizeMeta sizeT
  deriving (Eq, Show, Read)

-- | Expressions (RHS of an assignment operation)
data Expr sizeT
  = ConstE {val :: Value}
  | VarE {var :: Ident}
  | AddE {lhs, rhs :: Expr sizeT}
  | MulE {lhs, rhs :: Expr sizeT}
  | LEqE {lhs, rhs :: Expr sizeT}
  | AndE {lhs, rhs :: Expr sizeT}
  | NotE {arg :: Expr sizeT}
  | MinE {lhs, rhs :: Expr sizeT}
  deriving (Eq, Show, Read)

-- | Type of function call
data FunctionCall
  = FunctionCall Ident
  | OracleCall
  | UProcAndMeas Ident
  deriving (Eq, Show, Read)

-- | CQ Statement
data Stmt sizeT
  = SkipS
  | AssignS {rets :: [Ident], expr :: Expr sizeT}
  | RandomS {ret :: Ident, ty :: VarType sizeT}
  | CallS {fun :: FunctionCall, meta_params :: [MetaParam sizeT], args :: [Ident]}
  | SeqS [Stmt sizeT]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt sizeT}
  | RepeatS {n_iter :: sizeT, loop_body :: Stmt sizeT}
  | HoleS String
  | -- syntax sugar
    WhileK {n_iter :: sizeT, cond :: Ident, loop_body :: Stmt sizeT}
  | WhileKWithCondExpr {n_iter :: sizeT, cond :: Ident, cond_expr :: Expr sizeT, loop_body :: Stmt sizeT}
  deriving (Eq, Show, Read)

-- | CQ Procedure
data ProcDef sizeT = ProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_params :: [(Ident, VarType sizeT)]
  , proc_local_vars :: [(Ident, VarType sizeT)]
  , proc_body :: Stmt sizeT
  }
  deriving (Eq, Show, Read)

-- | CQ Oracle Def
newtype OracleDecl sizeT = OracleDecl
  { oracle_param_types :: [VarType sizeT]
  }
  deriving (Eq, Show, Read)

-- | CQ Program
data Program sizeT costT = Program
  { oracle_decl :: OracleDecl sizeT
  , proc_defs :: Ctx.Context (ProcDef sizeT)
  , uproc_defs :: Ctx.Context (UQPL.ProcDef sizeT costT)
  , stmt :: Stmt sizeT
  }
  deriving (Eq, Show, Read)

-- ================================================================================
-- Lenses
-- ================================================================================

instance HasAst (Stmt sizeT) where
  _ast focus (SeqS ss) = SeqS <$> traverse focus ss
  _ast focus (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> focus s_true <*> focus s_false
  _ast focus (RepeatS n_iter loop_body) = RepeatS n_iter <$> focus loop_body
  _ast _ s = pure s

instance HasStmt (Stmt sizeT) (Stmt sizeT) where
  _stmt = id

instance HasStmt (ProcDef sizeT) (Stmt sizeT) where
  _stmt focus (ProcDef proc_name proc_meta_params proc_params proc_local_vars proc_body) =
    ProcDef proc_name proc_meta_params proc_params proc_local_vars <$> focus proc_body

instance HasStmt (Program sizeT costT) (Stmt sizeT) where
  _stmt focus (Program o ps ups s) = Program o <$> traverse (_stmt focus) ps <*> pure ups <*> _stmt focus s

-- ================================================================================
-- Syntax Sugar
-- ================================================================================

-- | bounded while loop
whileK ::
  -- | iteration limit
  sizeT ->
  -- | loop condition
  Ident ->
  -- | loop body
  Stmt sizeT ->
  Stmt sizeT
whileK k cond body = RepeatS k $ IfThenElseS cond body SkipS

-- | bounded while loop given an expression for the loop condition
whileKWithCondExpr ::
  -- | iteration limit
  sizeT ->
  -- | loop condition variable
  Ident ->
  -- | loop condition expression
  Expr sizeT ->
  -- | loop body
  Stmt sizeT ->
  Stmt sizeT
whileKWithCondExpr k cond_var cond_expr body =
  SeqS
    [ compute_cond
    , whileK k cond_var (SeqS [body, compute_cond])
    ]
 where
  compute_cond = AssignS [cond_var] cond_expr

-- | Remove syntax sugar. Use with `rewriteAST`.
desugarS :: Stmt sizeT -> Maybe (Stmt sizeT)
desugarS WhileK{n_iter, cond, loop_body} = Just $ whileK n_iter cond loop_body
desugarS WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = Just $ whileKWithCondExpr n_iter cond cond_expr loop_body
desugarS _ = Nothing

-- ================================================================================
-- Code printing
-- ================================================================================

parenBinExpr :: (Show sizeT) => String -> Expr sizeT -> Expr sizeT -> String
parenBinExpr op_sym lhs rhs =
  "(" ++ unwords [toCodeString lhs, op_sym, toCodeString rhs] ++ ")"

instance (Show sizeT) => ToCodeString (Expr sizeT) where
  toCodeString ConstE{val} = show val
  toCodeString VarE{var} = var
  toCodeString AddE{lhs, rhs} = parenBinExpr "+" lhs rhs
  toCodeString MulE{lhs, rhs} = parenBinExpr "*" lhs rhs
  toCodeString LEqE{lhs, rhs} = parenBinExpr "<=" lhs rhs
  toCodeString AndE{lhs, rhs} = parenBinExpr "&&" lhs rhs
  toCodeString NotE{arg} = "!" ++ toCodeString arg
  toCodeString MinE{lhs, rhs} =
    "min(" ++ toCodeString lhs ++ ", " ++ toCodeString rhs ++ ")"

instance (Show sizeT) => ToCodeString (Stmt sizeT) where
  toCodeLines SkipS = ["skip;"]
  toCodeLines AssignS{rets, expr} =
    [printf "%s := %s;" (commaList rets) (toCodeString expr)]
  toCodeLines RandomS{ret, ty} =
    [printf "%s :=$ %s;" ret (toCodeString ty)]
  toCodeLines CallS{fun = FunctionCall f, args} =
    [printf "call %s(%s);" f (commaList args)]
  toCodeLines CallS{fun = OracleCall, args} =
    [printf "Oracle(%s);" (commaList args)]
  toCodeLines CallS{fun = UProcAndMeas uproc_id, args} =
    [printf "call_uproc_and_meas %s(%s);" uproc_id (commaList args)]
  toCodeLines (SeqS ss) = concatMap toCodeLines ss
  toCodeLines IfThenElseS{cond, s_true, s_false} =
    [printf "if (%s) then" cond]
      ++ indent (toCodeLines s_true)
      ++ ["else"]
      ++ indent (toCodeLines s_false)
      ++ ["end"]
  toCodeLines RepeatS{n_iter, loop_body} =
    [printf "repeat %s do" (show n_iter)]
      ++ indent (toCodeLines loop_body)
      ++ ["end"]
  -- hole
  toCodeLines (HoleS info) = [printf "HOLE :: %s;" info]
  -- syntax sugar
  toCodeLines WhileK{n_iter, cond, loop_body} =
    printf "while[%s] (%s) do" (show n_iter) cond
      : indent (toCodeLines loop_body)
      ++ ["end"]
  toCodeLines WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} =
    printf "while[%s] (%s := %s) do" (show n_iter) cond (toCodeString cond_expr)
      : indent (toCodeLines loop_body)
      ++ ["end"]

instance (Show sizeT) => ToCodeString (ProcDef sizeT) where
  toCodeLines ProcDef{proc_name, proc_meta_params, proc_params, proc_local_vars, proc_body} =
    [ printf
        "proc %s[%s](%s) { locals: (%s) } do"
        proc_name
        (commaList proc_meta_params)
        (commaList $ map showTypedVar proc_params)
        (commaList $ map showTypedVar proc_local_vars)
    ]
      ++ indent (toCodeLines proc_body)
      ++ ["end"]
   where
    showTypedVar (x, ty) = printf "%s: %s" x (toCodeString ty)

instance (Show sizeT, Show costT) => ToCodeString (Program sizeT costT) where
  toCodeLines Program{proc_defs, uproc_defs, stmt} =
    map toCodeString (Ctx.elems uproc_defs)
      ++ map toCodeString (Ctx.elems proc_defs)
      ++ [toCodeString stmt]

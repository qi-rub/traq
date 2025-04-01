module QCompose.CQPL.Syntax (
  VarType (..),
  Value (..),
  Expr (..),
  FunctionCall (..),
  Stmt (..),
  ProcDef (..),
  OracleDecl (..),
  Program (..),
) where

import Text.Printf (printf)

import QCompose.Prelude hiding (Value)
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

data VarType = IntT | FloatT
  deriving (Eq, Show, Read)

data Value
  = IntV Int
  | FloatV Float
  | SymV String
  deriving (Eq, Show, Read)

data Expr
  = ConstE {val :: Value}
  | VarE {var :: Ident}
  | AddE {lhs, rhs :: Expr}
  | MulE {lhs, rhs :: Expr}
  | LEqE {lhs, rhs :: Expr}
  | AndE {lhs, rhs :: Expr}
  | NotE {arg :: Expr}
  | MinE {lhs, rhs :: Expr}
  deriving (Eq, Show, Read)

data FunctionCall
  = FunctionCall Ident
  | OracleCall
  | UProcAndMeas Ident
  deriving (Eq, Show, Read)

data Stmt
  = SkipS
  | AssignS {rets :: [Ident], expr :: Expr}
  | RandomS {ret :: Ident, max_val :: Expr}
  | CallS {fun :: FunctionCall, args :: [Ident]}
  | SeqS [Stmt]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt}
  | ForS {iter_var :: Ident, iter_lim :: Expr, loop_body :: Stmt}
  | WhileS {cond_expr :: Expr, loop_body :: Stmt}
  | ReturnS {rets :: [Ident]}

data ProcDef a
  = ProcDef
      { proc_name :: Ident
      , proc_params :: [(Ident, VarType)]
      , proc_local_vars :: [(Ident, VarType)]
      , proc_body :: Stmt
      }
  | UQPLProcDef (UQPL.ProcDef a)

newtype OracleDecl = OracleDecl
  { oracle_param_types :: [VarType]
  }
  deriving (Eq, Show, Read)

data Program a = Program
  { oracle_decl :: OracleDecl
  , proc_defs :: [ProcDef a]
  , stmt :: Stmt
  }

instance ToCodeString VarType where
  toCodeString IntT = "int"
  toCodeString FloatT = "float"

parenBinExpr :: String -> Expr -> Expr -> String
parenBinExpr op_sym lhs rhs =
  "(" ++ unwords [toCodeString lhs, op_sym, toCodeString rhs] ++ ")"

instance ToCodeString Value where
  toCodeString (IntV v) = show v
  toCodeString (FloatV v) = show v
  toCodeString (SymV v) = v

instance ToCodeString Expr where
  toCodeString ConstE{val} = toCodeString val
  toCodeString VarE{var} = var
  toCodeString AddE{lhs, rhs} = parenBinExpr "+" lhs rhs
  toCodeString MulE{lhs, rhs} = parenBinExpr "*" lhs rhs
  toCodeString LEqE{lhs, rhs} = parenBinExpr "<=" lhs rhs
  toCodeString AndE{lhs, rhs} = parenBinExpr "&&" lhs rhs
  toCodeString NotE{arg} = "!" ++ toCodeString arg
  toCodeString MinE{lhs, rhs} =
    "min(" ++ toCodeString lhs ++ ", " ++ toCodeString rhs ++ ")"

instance ToCodeString Stmt where
  toCodeLines SkipS = []
  toCodeLines AssignS{rets, expr} =
    [printf "%s := %s;" (commaList rets) (toCodeString expr)]
  toCodeLines RandomS{ret, max_val} =
    [printf "%s :=$ %s;" ret (toCodeString max_val)]
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
  toCodeLines ForS{iter_var, iter_lim, loop_body} =
    [printf "for (%s in range(%s)) do" iter_var (toCodeString iter_lim)]
      ++ indent (toCodeLines loop_body)
      ++ ["end"]
  toCodeLines WhileS{cond_expr, loop_body} =
    [printf "while (%s) do" (toCodeString cond_expr)]
      ++ indent (toCodeLines loop_body)
      ++ ["end"]
  toCodeLines ReturnS{rets} =
    [printf "return %s;" (commaList rets)]

instance (Show a) => ToCodeString (ProcDef a) where
  toCodeLines ProcDef{proc_name, proc_params, proc_body} =
    [printf "proc %s(%s)" proc_name (commaList $ map fst proc_params)]
      ++ indent (toCodeLines proc_body)
      ++ ["end"]
  toCodeLines (UQPLProcDef uproc) = toCodeLines uproc

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{proc_defs, stmt} =
    map toCodeString proc_defs ++ [toCodeString stmt]

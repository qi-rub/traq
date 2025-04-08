module QCompose.CQPL.Syntax (
  -- * Syntax
  Value (..),
  Expr (..),
  FunctionCall (..),
  Stmt (..),
  ProcDef (..),
  OracleDecl (..),
  Program (..),

  -- * Syntax Sugar
  whileK,
  whileKWithCondExpr,
) where

import Text.Printf (printf)

import QCompose.Prelude hiding (Value)
import QCompose.ProtoLang (VarType)
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

-- ================================================================================
-- Syntax
-- ================================================================================

data Value
  = IntV Integer
  | FloatV Float
  | SymV String
  deriving (Eq, Show, Read)

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

data FunctionCall
  = FunctionCall Ident
  | OracleCall
  | UProcAndMeas Ident
  deriving (Eq, Show, Read)

data Stmt sizeT
  = SkipS
  | AssignS {rets :: [Ident], expr :: Expr sizeT}
  | RandomS {ret :: Ident, ty :: VarType sizeT}
  | CallS {fun :: FunctionCall, args :: [Ident]}
  | SeqS [Stmt sizeT]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt sizeT}
  | RepeatS {n_iter :: sizeT, loop_body :: Stmt sizeT}

data ProcDef sizeT = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, VarType sizeT)]
  , proc_local_vars :: [(Ident, VarType sizeT)]
  , proc_body :: Stmt sizeT
  }

newtype OracleDecl sizeT = OracleDecl
  { oracle_param_types :: [VarType sizeT]
  }
  deriving (Eq, Show, Read)

data Program sizeT = Program
  { oracle_decl :: OracleDecl sizeT
  , proc_defs :: [ProcDef sizeT]
  , uproc_defs :: [UQPL.ProcDef sizeT Float]
  , stmt :: Stmt sizeT
  }

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

-- ================================================================================
-- Code printing
-- ================================================================================

parenBinExpr :: (Show sizeT) => String -> Expr sizeT -> Expr sizeT -> String
parenBinExpr op_sym lhs rhs =
  "(" ++ unwords [toCodeString lhs, op_sym, toCodeString rhs] ++ ")"

instance ToCodeString Value where
  toCodeString (IntV v) = show v
  toCodeString (FloatV v) = show v
  toCodeString (SymV v) = v

instance (Show sizeT) => ToCodeString (Expr sizeT) where
  toCodeString ConstE{val} = toCodeString val
  toCodeString VarE{var} = var
  toCodeString AddE{lhs, rhs} = parenBinExpr "+" lhs rhs
  toCodeString MulE{lhs, rhs} = parenBinExpr "*" lhs rhs
  toCodeString LEqE{lhs, rhs} = parenBinExpr "<=" lhs rhs
  toCodeString AndE{lhs, rhs} = parenBinExpr "&&" lhs rhs
  toCodeString NotE{arg} = "!" ++ toCodeString arg
  toCodeString MinE{lhs, rhs} =
    "min(" ++ toCodeString lhs ++ ", " ++ toCodeString rhs ++ ")"

instance (Show sizeT) => ToCodeString (Stmt sizeT) where
  toCodeLines SkipS = []
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

instance (Show a) => ToCodeString (ProcDef a) where
  toCodeLines ProcDef{proc_name, proc_params, proc_body} =
    [printf "proc %s(%s)" proc_name (commaList $ map fst proc_params)]
      ++ indent (toCodeLines proc_body)
      ++ ["end"]

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{proc_defs, uproc_defs, stmt} =
    map toCodeString uproc_defs
      ++ map toCodeString proc_defs
      ++ [toCodeString stmt]

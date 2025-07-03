{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Syntax (
  -- * Syntax

  -- ** Expressions and Statements
  MetaParam (..),
  FunctionCall (..),
  Stmt (..),

  -- ** Procedures
  ProcBody (..),
  ProcDef (..),
  ProcCtx,
  Program (..),
  Program',

  -- * Syntax Sugar
  ifThenS,
  desugarS,
) where

import Data.Void (Void)
import Lens.Micro.GHC
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang (MetaParam (..), VarType)
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import Traq.Utils.ASTRewriting
import Traq.Utils.Printing

-- ================================================================================
-- Syntax
-- ================================================================================

-- | Type of function call
data FunctionCall
  = FunctionCall Ident
  | UProcAndMeas Ident
  deriving (Eq, Show, Read)

-- | CQ Statement
data Stmt holeT sizeT
  = SkipS
  | CommentS String
  | AssignS {rets :: [Ident], expr :: P.BasicExpr sizeT}
  | RandomS {ret :: Ident, max_val :: MetaParam sizeT}
  | RandomDynS {ret :: Ident, max_var :: Ident}
  | CallS {fun :: FunctionCall, meta_params :: [Either (MetaParam sizeT) Ident], args :: [Ident]}
  | SeqS [Stmt holeT sizeT]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt holeT sizeT}
  | RepeatS {n_iter :: MetaParam sizeT, loop_body :: Stmt holeT sizeT}
  | HoleS holeT
  | -- syntax sugar
    WhileK {n_iter :: MetaParam sizeT, cond :: Ident, loop_body :: Stmt holeT sizeT}
  | WhileKWithCondExpr {n_iter :: MetaParam sizeT, cond :: Ident, cond_expr :: P.BasicExpr sizeT, loop_body :: Stmt holeT sizeT}
  | ForInArray {loop_index :: Ident, loop_index_ty :: VarType sizeT, loop_values :: [P.BasicExpr sizeT], loop_body :: Stmt holeT sizeT}
  deriving (Eq, Show, Read)

ifThenS :: Ident -> Stmt holeT sizeT -> Stmt holeT sizeT
ifThenS cond s_true = IfThenElseS{cond, s_true, s_false = SkipS}

-- | CQ Procedure body: binds the parameters to names, and optionally uses local variables.
data ProcBody holeT sizeT = ProcBody
  { proc_param_names :: [Ident]
  , proc_local_vars :: [(Ident, VarType sizeT)]
  , proc_body_stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

-- | CQ Procedure
data ProcDef holeT sizeT costT = ProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [VarType sizeT]
  , proc_body_or_tick :: Either costT (ProcBody holeT sizeT) -- Left tick | Right body
  }
  deriving (Eq, Show, Read)

-- | CQ procedures
type ProcCtx holeT sizeT costT = Ctx.Context (ProcDef holeT sizeT costT)

-- | CQ Program
data Program holeT sizeT costT = Program
  { proc_defs :: ProcCtx holeT sizeT costT
  , uproc_defs :: UQPL.ProcCtx holeT sizeT costT
  , stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

-- | Alias without holes.
type Program' = Program Void

-- ================================================================================
-- Lenses
-- ================================================================================

instance HasAst (Stmt holeT sizeT) where
  _ast focus (SeqS ss) = SeqS <$> traverse focus ss
  _ast focus (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> focus s_true <*> focus s_false
  _ast focus (RepeatS n_iter loop_body) = RepeatS n_iter <$> focus loop_body
  _ast _ s = pure s

instance HasStmt (Stmt holeT sizeT) (Stmt holeT sizeT) where
  _stmt = id

instance HasStmt (ProcBody holeT sizeT) (Stmt holeT sizeT) where
  _stmt focus proc_body = (\s' -> proc_body{proc_body_stmt = s'}) <$> focus (proc_body_stmt proc_body)

instance HasStmt (ProcDef holeT sizeT costT) (Stmt holeT sizeT) where
  _stmt focus proc_def@ProcDef{proc_body_or_tick = Right proc_body} =
    _stmt focus proc_body <&> \proc_body' -> proc_def{proc_body_or_tick = Right proc_body'}
  _stmt _ proc_def = pure proc_def

instance HasStmt (Program holeT sizeT costT) (Stmt holeT sizeT) where
  _stmt focus (Program proc_defs uproc_defs stmt) = Program <$> traverse (_stmt focus) proc_defs <*> pure uproc_defs <*> _stmt focus stmt

-- ================================================================================
-- Syntax Sugar
-- ================================================================================

-- | bounded while loop
whileK ::
  -- | iteration limit
  MetaParam sizeT ->
  -- | loop condition
  Ident ->
  -- | loop body
  Stmt holeT sizeT ->
  Stmt holeT sizeT
whileK k cond body = RepeatS k $ IfThenElseS cond body SkipS

-- | bounded while loop given an expression for the loop condition
whileKWithCondExpr ::
  -- | iteration limit
  MetaParam sizeT ->
  -- | loop condition variable
  Ident ->
  -- | loop condition expression
  P.BasicExpr sizeT ->
  -- | loop body
  Stmt holeT sizeT ->
  Stmt holeT sizeT
whileKWithCondExpr k cond_var cond_expr body =
  SeqS
    [ compute_cond
    , whileK k cond_var (SeqS [body, compute_cond])
    ]
 where
  compute_cond = AssignS [cond_var] cond_expr

forInArray :: Ident -> VarType sizeT -> [P.BasicExpr sizeT] -> Stmt holeT sizeT -> Stmt holeT sizeT
forInArray i _ty ix_vals s =
  SeqS
    [ SeqS
        [ AssignS{rets = [i], expr = v}
        , s
        ]
    | v <- ix_vals
    ]

-- | Remove syntax sugar. Use with `rewriteAST`.
desugarS :: Stmt holeT sizeT -> Maybe (Stmt holeT sizeT)
desugarS WhileK{n_iter, cond, loop_body} = Just $ whileK n_iter cond loop_body
desugarS WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = Just $ whileKWithCondExpr n_iter cond cond_expr loop_body
desugarS ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = Just $ forInArray loop_index loop_index_ty loop_values loop_body
desugarS _ = Nothing

-- ================================================================================
-- Code printing
-- ================================================================================

instance (Show holeT, Show sizeT) => ToCodeString (Stmt holeT sizeT) where
  toCodeLines SkipS = ["skip;"]
  toCodeLines AssignS{rets, expr} =
    [printf "%s := %s;" (commaList rets) (toCodeString expr)]
  toCodeLines RandomS{ret, max_val} =
    [printf "%s :=$ [1 .. %s];" ret (toCodeString max_val)]
  toCodeLines RandomDynS{ret, max_var} =
    [printf "%s :=$ [1 .. %s];" ret max_var]
  toCodeLines CallS{fun, meta_params, args} =
    [printf "%s[%s](%s);" (f_str fun) meta_params_str (commaList args)]
   where
    f_str :: FunctionCall -> String
    f_str (FunctionCall fname) = printf "call %s" fname
    f_str (UProcAndMeas uproc_id) = printf "call_uproc_and_meas %s" uproc_id

    meta_params_str = commaList $ map (either toCodeString id) meta_params
  toCodeLines (SeqS ss) = concatMap toCodeLines ss
  toCodeLines IfThenElseS{cond, s_true, s_false} =
    [printf "if (%s) then" cond]
      ++ indent (toCodeLines s_true)
      ++ ["else"]
      ++ indent (toCodeLines s_false)
      ++ ["end"]
  toCodeLines RepeatS{n_iter, loop_body} =
    [printf "repeat %s do" (toCodeString n_iter)]
      ++ indent (toCodeLines loop_body)
      ++ ["end"]
  -- hole
  toCodeLines (HoleS info) = [printf "HOLE :: %s;" (show info)]
  -- syntax sugar
  toCodeLines WhileK{n_iter, cond, loop_body} =
    printf "while[%s] (%s) do" (toCodeString n_iter) cond
      : indent (toCodeLines loop_body)
      ++ ["end"]
  toCodeLines WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} =
    printf "while[%s] (%s := %s) do" (toCodeString n_iter) cond (toCodeString cond_expr)
      : indent (toCodeLines loop_body)
      ++ ["end"]
  toCodeLines (CommentS c) = ["// " ++ c]
  toCodeLines ForInArray{loop_index, loop_values, loop_body} =
    printf "for %s in [%s] do" loop_index (commaList $ map toCodeString loop_values)
      : indent (toCodeLines loop_body)
      ++ ["end"]

instance (Show holeT, Show sizeT, Show costT) => ToCodeString (ProcDef holeT sizeT costT) where
  toCodeLines ProcDef{proc_name, proc_meta_params, proc_param_types, proc_body_or_tick = Left tick} =
    [ printf
        "proc %s[%s](%s) :: tick(%s);"
        proc_name
        (commaList proc_meta_params)
        (commaList $ zipWith showTypedIxVar [1 :: Int ..] proc_param_types)
        (show tick)
    ]
   where
    showTypedIxVar i ty = printf "x_%d: %s" i (toCodeString ty)
  toCodeLines ProcDef{proc_name, proc_meta_params, proc_param_types, proc_body_or_tick = Right ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}} =
    [ printf
        "proc %s[%s](%s) { locals: (%s) } do"
        proc_name
        (commaList proc_meta_params)
        (commaList $ zipWith showTypedVar proc_param_names proc_param_types)
        (commaList $ map (uncurry showTypedVar) proc_local_vars)
    ]
      ++ indent (toCodeLines proc_body_stmt)
      ++ ["end"]
   where
    showTypedVar x ty = printf "%s: %s" x (toCodeString ty)

instance (Show holeT, Show sizeT, Show costT) => ToCodeString (Program holeT sizeT costT) where
  toCodeLines Program{proc_defs, uproc_defs, stmt} =
    map toCodeString (Ctx.elems uproc_defs)
      ++ map toCodeString (Ctx.elems proc_defs)
      ++ [toCodeString stmt]

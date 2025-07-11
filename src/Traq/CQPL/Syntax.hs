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

  -- * Lenses
  HasProcCtx (..),
) where

import Control.Monad (zipWithM, (>=>))
import Data.Void (Void)
import Lens.Micro.GHC
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang (VarType)
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import Traq.Utils.ASTRewriting
import qualified Traq.Utils.Printing as PP

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

type instance SizeType (Stmt holeT sizeT) = sizeT
type instance HoleType (Stmt holeT sizeT) = holeT

ifThenS :: Ident -> Stmt holeT sizeT -> Stmt holeT sizeT
ifThenS cond s_true = IfThenElseS{cond, s_true, s_false = SkipS}

-- | CQ Procedure body: binds the parameters to names, and optionally uses local variables.
data ProcBody holeT sizeT = ProcBody
  { proc_param_names :: [Ident]
  , proc_local_vars :: [(Ident, VarType sizeT)]
  , proc_body_stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

type instance SizeType (ProcBody holeT sizeT) = sizeT
type instance HoleType (ProcBody holeT sizeT) = holeT

-- | CQ Procedure
data ProcDef holeT sizeT costT = ProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [VarType sizeT]
  , proc_body_or_tick :: Either costT (ProcBody holeT sizeT) -- Left tick | Right body
  }
  deriving (Eq, Show, Read)

type instance SizeType (ProcDef holeT sizeT costT) = sizeT
type instance HoleType (ProcDef holeT sizeT costT) = holeT
type instance CostType (ProcDef holeT sizeT costT) = costT

-- | CQ procedures
type ProcCtx holeT sizeT costT = Ctx.Context (ProcDef holeT sizeT costT)

type instance SizeType (ProcCtx holeT sizeT costT) = sizeT
type instance CostType (ProcCtx holeT sizeT costT) = costT
type instance HoleType (ProcCtx holeT sizeT costT) = holeT

class HasProcCtx s where
  _procCtx :: (holeT ~ HoleType s, sizeT ~ SizeType s, costT ~ CostType s) => Lens' s (ProcCtx holeT sizeT costT)

-- | CQ Program
data Program holeT sizeT costT = Program
  { proc_defs :: ProcCtx holeT sizeT costT
  , uproc_defs :: UQPL.ProcCtx holeT sizeT costT
  , stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

type instance SizeType (Program holeT sizeT costT) = sizeT
type instance CostType (Program holeT sizeT costT) = costT
type instance HoleType (Program holeT sizeT costT) = holeT

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
    [ SeqS [AssignS{rets = [i], expr = v}, s]
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

instance (Show holeT, Show sizeT) => PP.ToCodeString (Stmt holeT sizeT) where
  build SkipS = PP.putLine "skip;"
  build AssignS{rets, expr} = do
    e_s <- PP.fromBuild expr
    PP.putLine $ printf "%s := %s;" (PP.commaList rets) e_s
  build RandomS{ret, max_val} = do
    max_val_s <- PP.fromBuild max_val
    PP.putLine $ printf "%s :=$ [1 .. %s];" ret max_val_s
  build RandomDynS{ret, max_var} =
    PP.putLine $ printf "%s :=$ [1 .. %s];" ret max_var
  build CallS{fun, meta_params, args} = do
    meta_params_str <- PP.commaList <$> mapM (either PP.fromBuild return) meta_params
    PP.putLine $ printf "%s[%s](%s);" (f_str fun) meta_params_str (PP.commaList args)
   where
    f_str :: FunctionCall -> String
    f_str (FunctionCall fname) = printf "call %s" fname
    f_str (UProcAndMeas uproc_id) = printf "call_uproc_and_meas %s" uproc_id
  build (SeqS ss) = mapM_ PP.build ss
  build IfThenElseS{cond, s_true, s_false} = do
    PP.putLine $ printf "if (%s) {" cond
    PP.indented $ PP.build s_true
    PP.putLine "} else {"
    PP.indented $ PP.build s_false
    PP.putLine "}"
  build RepeatS{n_iter, loop_body} = do
    header <- printf "repeat (%s)" <$> PP.fromBuild n_iter
    PP.bracedBlockWith header $ PP.build loop_body
  -- hole
  build (HoleS info) = PP.putLine $ printf "HOLE :: %s;" (show info)
  -- syntax sugar
  build WhileK{n_iter, cond, loop_body} = do
    n_iter_s <- PP.fromBuild n_iter
    PP.putLine $ printf "while[%s] (%s) {" n_iter_s cond
    PP.indented $ PP.build loop_body
    PP.putLine "}"
  build WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = do
    printf "while[%s] (%s := %s) do" <$> PP.fromBuild n_iter <*> pure cond <*> PP.fromBuild cond_expr
      >>= PP.putLine
    PP.indented $ PP.build loop_body
    PP.putLine "}"
  build (CommentS c) = PP.putComment c
  build ForInArray{loop_index, loop_values, loop_body} = do
    loop_values_s <- PP.commaList <$> mapM PP.fromBuild loop_values
    PP.delimitedBlock
      (printf "for (%s in [%s]) {" loop_index loop_values_s)
      "}"
      $ PP.build loop_body

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (ProcDef holeT sizeT costT) where
  build ProcDef{proc_name, proc_meta_params, proc_param_types, proc_body_or_tick = Left tick} = do
    let showTypedIxVar i ty = printf "x_%d: %s" i <$> PP.fromBuild ty
    param_tys_s <- PP.commaList <$> zipWithM showTypedIxVar [1 :: Int ..] proc_param_types
    PP.putLine $
      printf
        "proc %s[%s](%s) :: tick(%s);"
        proc_name
        (PP.commaList proc_meta_params)
        param_tys_s
        (show tick)
  build ProcDef{proc_name, proc_meta_params, proc_param_types, proc_body_or_tick = Right ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}} = do
    header <- PP.listenWord . PP.concatenated $ do
      -- proc name and meta-params
      PP.putWord $ "proc " <> proc_name
      PP.putWord $ PP.wrapNonEmpty "[" "]" $ PP.commaList proc_meta_params

      -- arguments
      let showTypedVar x ty = printf "%s: %s" x <$> PP.fromBuild ty
      PP.putWord . printf "(%s)" . PP.commaList =<< zipWithM showTypedVar proc_param_names proc_param_types

      -- local vars
      PP.putWord . printf " { locals: (%s) } do" . PP.commaList =<< mapM (uncurry showTypedVar) proc_local_vars

    -- emit the body
    PP.bracedBlockWith header $ PP.build proc_body_stmt

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (Program holeT sizeT costT) where
  build Program{proc_defs, uproc_defs, stmt} = do
    mapM_ (PP.build >=> const PP.endl) $ Ctx.elems uproc_defs
    mapM_ (PP.build >=> const PP.endl) $ Ctx.elems proc_defs
    PP.build stmt

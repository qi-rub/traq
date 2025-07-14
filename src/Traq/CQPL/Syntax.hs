{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Syntax (
  -- * Syntax
  MetaParam (..),

  -- ** Unitary Fragment

  -- ** Classical Fragment
  FunctionCall (..),
  Stmt (..),

  -- ** Procedures
  ParamTag (..),
  UProcBody (..),
  CProcBody (..),
  ProcBody (..),
  ProcDef (..),

  -- ** Program
  ProcCtx,
  Program (..),
  Program',

  -- * Syntax Sugar
  ifThenS,
  desugarS,

  -- * Lenses
  HasProcCtx (..),
  isUProc,
  isCProc,
) where

import Control.Monad ((>=>))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Writer (MonadWriter)
import Data.Void (Void)
import Lens.Micro.GHC
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang (MetaParam (..), VarType)
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import Traq.Utils.ASTRewriting
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Syntax
-- ================================================================================

-- ================================================================================
-- Classical Statements
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
    meta_params_str <- PP.wrapNonEmpty "[" "]" . PP.commaList <$> mapM (either PP.fromBuild return) meta_params
    PP.putLine $ printf "%s%s(%s);" (f_str fun) meta_params_str (PP.commaList args)
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

-- ================================================================================
-- Procedures
-- ================================================================================

data ParamTag = ParamCtrl | ParamInp | ParamOut | ParamAux | ParamUnk deriving (Eq, Show, Read, Enum)

instance PP.ToCodeString ParamTag where
  build ParamCtrl = PP.putWord " CTRL"
  build ParamInp = PP.putWord " IN"
  build ParamOut = PP.putWord " OUT"
  build ParamAux = PP.putWord " AUX"
  build ParamUnk = PP.putWord ""

-- | Unitary Procedure body: either a statement (with parameter name bindings) or a tick.
data UProcBody holeT sizeT costT
  = UProcBody
      { uproc_param_names :: [Ident]
      , uproc_param_tags :: [ParamTag]
      , uproc_body_stmt :: UQPL.UStmt holeT sizeT
      }
  | UProcDecl {utick :: costT}
  deriving (Eq, Show, Read)

type instance SizeType (UProcBody holeT sizeT costT) = sizeT
type instance HoleType (UProcBody holeT sizeT costT) = holeT
type instance CostType (UProcBody holeT sizeT costT) = costT

buildUProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT, Show costT, Show holeT) =>
  UProcBody holeT sizeT costT ->
  String ->
  [String] ->
  m ()
buildUProcBody UProcDecl{utick} name param_tys = do
  PP.putLine $ printf "uproc %s(%s) :: tick(%s)" name (PP.commaList param_tys) (show utick)
buildUProcBody UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} name param_tys = do
  arg_list <-
    uproc_param_tags
      & mapM PP.fromBuild
      <&> zip3 uproc_param_names param_tys
      <&> map (\(x, ty, tag) -> printf "%s :%s %s" x tag ty)
  let header = printf "uproc %s(%s)" name $ PP.commaList arg_list
  PP.bracedBlockWith header $ PP.build uproc_body_stmt

-- | Classical Procedure body: either a tick, or statement with bindings for parameters names, and optionally using local variables.
data CProcBody holeT sizeT costT
  = CProcBody
      { cproc_param_names :: [Ident]
      , cproc_local_vars :: [(Ident, VarType sizeT)]
      , cproc_body_stmt :: Stmt holeT sizeT
      }
  | CProcDecl {ctick :: costT}
  deriving (Eq, Show, Read)

buildCProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT, Show costT, Show holeT) =>
  CProcBody holeT sizeT costT ->
  String ->
  [String] ->
  m ()
buildCProcBody CProcDecl{ctick} name param_tys = do
  PP.putLine $ printf "proc %s(%s) :: tick(%s)" name (PP.commaList param_tys) (show ctick)
buildCProcBody CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} name param_tys = do
  let arg_list = zipWith (printf "%s : %s") cproc_param_names param_tys

  local_list <-
    cproc_local_vars
      & traverse (_2 PP.fromBuild)
      <&> map (uncurry $ printf "%s : %s")

  let header =
        printf
          "proc %s(%s) { locals : (%s) }"
          name
          (PP.commaList arg_list)
          (PP.commaList local_list)
  PP.bracedBlockWith header $ PP.build cproc_body_stmt

type instance SizeType (CProcBody holeT sizeT costT) = sizeT
type instance CostType (CProcBody holeT sizeT costT) = costT
type instance HoleType (CProcBody holeT sizeT costT) = holeT

data ProcBody holeT sizeT costT
  = ProcBodyU (UProcBody holeT sizeT costT)
  | ProcBodyC (CProcBody holeT sizeT costT)
  deriving (Eq, Read, Show)

class ClassifyProc p where
  isUProc :: p -> Bool
  isCProc :: p -> Bool

instance ClassifyProc (ProcBody holeT sizeT costT) where
  isUProc (ProcBodyU _) = True
  isUProc _ = False

  isCProc (ProcBodyC _) = True
  isCProc _ = False

type instance SizeType (ProcBody holeT sizeT costT) = sizeT
type instance CostType (ProcBody holeT sizeT costT) = costT
type instance HoleType (ProcBody holeT sizeT costT) = holeT

buildProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT, Show costT, Show holeT) =>
  ProcBody holeT sizeT costT ->
  String ->
  [String] ->
  m ()
buildProcBody (ProcBodyU p) = buildUProcBody p
buildProcBody (ProcBodyC p) = buildCProcBody p

data ProcDef holeT sizeT costT
  = ProcDef
  { info_comment :: String
  , proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [VarType sizeT]
  , proc_body :: ProcBody holeT sizeT costT
  }
  deriving (Eq, Read, Show)

instance ClassifyProc (ProcDef holeT sizeT costT) where
  isUProc = isUProc . proc_body
  isCProc = isCProc . proc_body

type instance SizeType (ProcDef holeT sizeT costT) = sizeT
type instance HoleType (ProcDef holeT sizeT costT) = holeT
type instance CostType (ProcDef holeT sizeT costT) = costT

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (ProcDef holeT sizeT costT) where
  build ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} = do
    PP.putComment info_comment

    let full_proc_name =
          printf
            "%s%s"
            proc_name
            (PP.wrapNonEmpty "[" "]" $ PP.commaList proc_meta_params)

    ptys <- mapM (PP.listenWord . PP.build) proc_param_types

    buildProcBody proc_body full_proc_name ptys

-- ================================================================================
-- Program
-- ================================================================================

-- | CQ procedures
type ProcCtx holeT sizeT costT = Ctx.Context (ProcDef holeT sizeT costT)

class HasProcCtx s where
  _procCtx :: (holeT ~ HoleType s, sizeT ~ SizeType s, costT ~ CostType s) => Lens' s (ProcCtx holeT sizeT costT)

instance HasProcCtx (ProcCtx holeT sizeT costT) where _procCtx = id

-- | CQ Program
newtype Program holeT sizeT costT = Program
  { proc_defs :: ProcCtx holeT sizeT costT
  }
  deriving (Eq, Show, Read)

type instance SizeType (Program holeT sizeT costT) = sizeT
type instance CostType (Program holeT sizeT costT) = costT
type instance HoleType (Program holeT sizeT costT) = holeT

-- | Alias without holes.
type Program' = Program Void

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (Program holeT sizeT costT) where
  build Program{proc_defs} = do
    mapM_ (PP.build >=> const PP.endl) $ Ctx.elems proc_defs

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

instance HasStmt (CProcBody holeT sizeT costT) (Stmt holeT sizeT) where
  _stmt focus b@CProcBody{cproc_body_stmt} = focus cproc_body_stmt <&> \s' -> b{cproc_body_stmt = s'}
  _stmt _ b@CProcDecl{} = pure b

instance HasStmt (ProcDef holeT sizeT costT) (Stmt holeT sizeT) where
  _stmt focus p@ProcDef{proc_body = ProcBodyC b} =
    _stmt focus b <&> \b' -> p{proc_body = ProcBodyC b'}
  _stmt _ p = pure p

instance HasStmt (Program holeT sizeT costT) (Stmt holeT sizeT) where
  _stmt focus (Program proc_defs) = Program <$> traverse (_stmt focus) proc_defs

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
class CanDesugar p where
  desugarS :: p -> Maybe p

instance CanDesugar (Stmt holeT sizeT) where
  desugarS WhileK{n_iter, cond, loop_body} = Just $ whileK n_iter cond loop_body
  desugarS WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = Just $ whileKWithCondExpr n_iter cond cond_expr loop_body
  desugarS ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = Just $ forInArray loop_index loop_index_ty loop_values loop_body
  desugarS _ = Nothing

instance CanDesugar (UQPL.UStmt holeT sizeT) where
  desugarS UQPL.UWithComputedS{UQPL.with_ustmt, UQPL.body_ustmt} = Just $ UQPL.USeqS [with_ustmt, body_ustmt, UQPL.adjoint with_ustmt]
  desugarS _ = Nothing

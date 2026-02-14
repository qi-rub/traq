{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Syntax (
  -- * Syntax
  MetaParam (..),
  Arg (..),

  -- ** Unitary Fragment
  HasAdjoint (..),
  BasicGate (..),
  Unitary (..),
  UStmt (..),

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

  -- * Syntax Sugar
  ifThenS,
  mkForInRangeS,
  desugarS,

  -- * Lenses
  HasProcCtx (..),
  isUProc,
  isCProc,
) where

import Control.Monad ((>=>))
import Control.Monad.Writer (MonadWriter)
import Text.Printf (printf)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang (MetaParam (..), VarType)
import qualified Traq.ProtoLang as P
import Traq.Utils.ASTRewriting
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Common
-- ================================================================================

-- | An argument (to ops/procs)
data Arg size
  = Arg Ident -- variable
  | ArrElemArg (Arg size) (MetaParam size) -- array element
  deriving (Eq, Read, Show)

instance (Show size) => PP.ToCodeString (Arg size) where
  build (Arg x) = PP.putWord x
  build (ArrElemArg arg i) = do
    x <- PP.fromBuild arg
    i' <- PP.fromBuild i
    PP.putWord $ x ++ "[" ++ i' ++ "]"

-- ================================================================================
-- Unitary Fragment
-- ================================================================================
class HasAdjoint a where
  adjoint :: a -> a

-- --------------------------------------------------------------------------------
-- Unitary Operators
-- --------------------------------------------------------------------------------

data BasicGate
  = Toffoli
  | CNOT
  | XGate
  | HGate
  | COPY
  | SWAP
  deriving (Eq, Show, Read)

instance PP.ToCodeString BasicGate where
  build XGate = PP.putWord "X"
  build HGate = PP.putWord "H"
  build g = PP.putWord $ show g

isSelfAdjoint :: BasicGate -> Bool
isSelfAdjoint _ = True

-- | Unitary operators in CQPL
data Unitary sizeT
  = BasicGateU BasicGate
  | -- | reflect about |0>_T
    Refl0
  | RevEmbedU [Ident] (P.BasicExpr sizeT)
  | DistrU (P.DistrExpr sizeT)
  | Controlled (Unitary sizeT)
  | Adjoint (Unitary sizeT)
  deriving (Eq, Show, Read)

type instance SizeType (Unitary sizeT) = sizeT

instance (Show sizeT) => PP.ToCodeString (Unitary sizeT) where
  build (BasicGateU g) = PP.build g
  build (RevEmbedU xs e) = do
    e_s <- PP.fromBuild e
    PP.putWord $ printf "Embed[(%s) => %s]" (PP.commaList xs) e_s
  build (DistrU mu) = do
    e_s <- PP.fromBuild mu
    PP.putWord $ printf "Distr[%s]" e_s
  build Refl0 = PP.putWord $ printf "Refl0"
  build (Controlled u) = PP.putWord . ("Ctrl-" <>) =<< PP.fromBuild u
  build (Adjoint u) = PP.putWord . ("Adj-" <>) =<< PP.fromBuild u

instance HasAdjoint (Unitary sizeT) where
  adjoint u@(BasicGateU g) | isSelfAdjoint g = u
  adjoint Refl0 = Refl0
  adjoint u@(RevEmbedU _ _) = u
  adjoint (Controlled u) = Controlled (adjoint u)
  adjoint (Adjoint u) = u
  adjoint u = Adjoint u

-- --------------------------------------------------------------------------------
-- Unitary Statements
-- --------------------------------------------------------------------------------

-- | Unitary Statement
data UStmt sizeT
  = USkipS
  | UnitaryS {qargs :: [Arg sizeT], unitary :: Unitary sizeT} -- q... *= U
  | UCallS {uproc_id :: Ident, dagger :: Bool, qargs :: [Arg sizeT]} -- call F(q...)
  | USeqS [UStmt sizeT] -- W1; W2; ...
  | -- placeholders
    UCommentS String
  | -- syntax sugar
    URepeatS {n_iter :: P.MetaParam sizeT, uloop_body :: UStmt sizeT} -- repeat k do S;
  | UForInRangeS
      { iter_meta_var :: Ident
      , iter_lim :: P.MetaParam sizeT
      , uloop_body :: UStmt sizeT
      , dagger :: Bool
      }
  | UWithComputedS {with_ustmt, body_ustmt :: UStmt sizeT}
  deriving (Eq, Show, Read)

type instance SizeType (UStmt sizeT) = sizeT

mkForInRangeS :: Ident -> P.MetaParam sizeT -> UStmt sizeT -> UStmt sizeT
mkForInRangeS iter_meta_var iter_lim uloop_body = UForInRangeS{iter_meta_var, iter_lim, uloop_body, dagger = False}

instance HasAdjoint (UStmt sizeT) where
  adjoint s@(UCommentS _) = s
  adjoint USkipS = USkipS
  adjoint s@UCallS{dagger} = s{dagger = not dagger}
  adjoint (USeqS ss) = USeqS . reverse $ map adjoint ss
  adjoint s@UnitaryS{unitary} = s{unitary = adjoint unitary}
  adjoint (URepeatS k s) = URepeatS k (adjoint s)
  adjoint s@UForInRangeS{dagger} = s{dagger = not dagger}
  adjoint s@UWithComputedS{body_ustmt = b} = s{body_ustmt = adjoint b}

showDagger :: Bool -> String
showDagger True = "-adj"
showDagger False = ""

instance (Show sizeT) => PP.ToCodeString (UStmt sizeT) where
  build USkipS = PP.putLine "skip;"
  build (UCommentS c) = PP.putComment c
  build UnitaryS{qargs, unitary} = PP.concatenated $ do
    qs <- mapM PP.fromBuild qargs
    PP.putWord $ PP.commaList qs
    PP.putWord " *= "
    PP.build unitary
    PP.putWord ";"
  build UCallS{uproc_id, dagger, qargs} = PP.concatenated $ do
    qs <- mapM PP.fromBuild qargs
    PP.putWord "call"
    PP.putWord $ showDagger dagger
    PP.putWord $ printf " %s(%s);" uproc_id $ PP.commaList qs
  build (USeqS ps) = mapM_ PP.build ps
  -- syntax sugar
  build (URepeatS k s) = do
    header <- printf "repeat (%s)" <$> PP.fromBuild k
    PP.bracedBlockWith header $ PP.build s
  build UWithComputedS{with_ustmt, body_ustmt} = do
    PP.bracedBlockWith "with" $ PP.build with_ustmt
    PP.bracedBlockWith "do" $ PP.build body_ustmt
  build UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = do
    iter_lim_s <- PP.fromBuild iter_lim
    let ss = printf range_str iter_lim_s :: String
    let header = printf "for (#%s in %s)" iter_meta_var ss
    PP.bracedBlockWith header $ PP.build uloop_body
   where
    range_str :: String
    range_str | dagger = "%s - 1 .. 0" | otherwise = "0 .. < %s"

-- ================================================================================
-- Classical Statements
-- ================================================================================

-- | Type of function call
data FunctionCall
  = FunctionCall Ident
  | UProcAndMeas Ident
  deriving (Eq, Show, Read)

-- | CQ Statement
data Stmt sizeT
  = SkipS
  | CommentS String
  | AssignS {rets :: [Ident], expr :: P.BasicExpr sizeT}
  | RandomS {rets :: [Ident], distr_expr :: P.DistrExpr sizeT}
  | RandomDynS {ret :: Ident, max_var :: Ident}
  | CallS {fun :: FunctionCall, meta_params :: [Either (MetaParam sizeT) Ident], args :: [Arg sizeT]}
  | SeqS [Stmt sizeT]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt sizeT}
  | RepeatS {n_iter :: MetaParam sizeT, loop_body :: Stmt sizeT}
  | -- syntax sugar
    WhileK {n_iter :: MetaParam sizeT, cond :: Ident, loop_body :: Stmt sizeT}
  | WhileKWithCondExpr {n_iter :: MetaParam sizeT, cond :: Ident, cond_expr :: P.BasicExpr sizeT, loop_body :: Stmt sizeT}
  | ForInArray {loop_index :: Ident, loop_index_ty :: VarType sizeT, loop_values :: [P.BasicExpr sizeT], loop_body :: Stmt sizeT}
  | ForInRangeS {iter_meta_var :: Ident, iter_lim :: P.MetaParam sizeT, loop_body :: Stmt sizeT}
  deriving (Eq, Show, Read)

type instance SizeType (Stmt sizeT) = sizeT

ifThenS :: Ident -> Stmt sizeT -> Stmt sizeT
ifThenS cond s_true = IfThenElseS{cond, s_true, s_false = SkipS}

instance (Show sizeT) => PP.ToCodeString (Stmt sizeT) where
  build SkipS = PP.putLine "skip;"
  build AssignS{rets, expr} = do
    e_s <- PP.fromBuild expr
    PP.putLine $ printf "%s := %s;" (PP.commaList rets) e_s
  build RandomS{rets, distr_expr} = do
    distr_s <- PP.fromBuild distr_expr
    PP.putLine $ printf "%s :=$ %s;" (PP.commaList rets) distr_s
  build RandomDynS{ret, max_var} =
    PP.putLine $ printf "%s :=$ [1 .. %s];" ret max_var
  build CallS{fun, meta_params, args} = do
    meta_params_str <- PP.wrapNonEmpty "[" "]" . PP.commaList <$> mapM (either PP.fromBuild return) meta_params
    args_str <- mapM PP.fromBuild args
    PP.putLine $ printf "%s%s(%s);" (f_str fun) meta_params_str (PP.commaList args_str)
   where
    f_str :: FunctionCall -> String
    f_str (FunctionCall fname) = printf "call %s" fname
    f_str (UProcAndMeas uproc_id) = printf "meas %s" uproc_id
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
  build ForInRangeS{iter_meta_var, iter_lim, loop_body} = do
    n <- PP.fromBuild iter_lim
    PP.bracedBlockWith
      (printf "for (%s in 0 .. < %s)" iter_meta_var n)
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
data UProcBody sizeT
  = UProcBody
      { uproc_param_names :: [Ident]
      , uproc_param_tags :: [ParamTag]
      , uproc_body_stmt :: UStmt sizeT
      }
  | UProcDecl
  deriving (Eq, Show, Read)

type instance SizeType (UProcBody sizeT) = sizeT

buildUProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT) =>
  UProcBody sizeT ->
  String ->
  [String] ->
  m ()
buildUProcBody UProcDecl name param_tys = do
  PP.putLine $ printf "ext uproc %s(%s);" name (PP.commaList param_tys)
buildUProcBody UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} name param_tys = do
  arg_list <-
    uproc_param_tags
      & mapM PP.fromBuild
      <&> zip3 uproc_param_names param_tys
      <&> map (\(x, ty, tag) -> printf "%s :%s %s" x tag ty)
  let header = printf "uproc %s(%s)" name $ PP.commaList arg_list
  PP.bracedBlockWith header $ PP.build uproc_body_stmt

-- | Classical Procedure body: either a tick, or statement with bindings for parameters names, and optionally using local variables.
data CProcBody sizeT
  = CProcBody
      { cproc_param_names :: [Ident]
      , cproc_local_vars :: [(Ident, VarType sizeT)]
      , cproc_body_stmt :: Stmt sizeT
      }
  | CProcDecl
  deriving (Eq, Show, Read)

buildCProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT) =>
  CProcBody sizeT ->
  String ->
  [String] ->
  m ()
buildCProcBody CProcDecl name param_tys = do
  PP.putLine $ printf "ext proc %s(%s);" name (PP.commaList param_tys)
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

type instance SizeType (CProcBody sizeT) = sizeT

data ProcBody sizeT
  = ProcBodyU (UProcBody sizeT)
  | ProcBodyC (CProcBody sizeT)
  deriving (Eq, Read, Show)

class ClassifyProc p where
  isUProc :: p -> Bool
  isCProc :: p -> Bool

instance ClassifyProc (ProcBody sizeT) where
  isUProc (ProcBodyU _) = True
  isUProc _ = False

  isCProc (ProcBodyC _) = True
  isCProc _ = False

type instance SizeType (ProcBody sizeT) = sizeT

buildProcBody ::
  (MonadWriter [String] m, MonadFail m, Show sizeT) =>
  ProcBody sizeT ->
  String ->
  [String] ->
  m ()
buildProcBody (ProcBodyU p) = buildUProcBody p
buildProcBody (ProcBodyC p) = buildCProcBody p

data ProcDef sizeT
  = ProcDef
  { info_comment :: String
  , proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [VarType sizeT]
  , proc_body :: ProcBody sizeT
  }
  deriving (Eq, Read, Show)

instance ClassifyProc (ProcDef sizeT) where
  isUProc = isUProc . proc_body
  isCProc = isCProc . proc_body

type instance SizeType (ProcDef sizeT) = sizeT

instance (Show sizeT) => PP.ToCodeString (ProcDef sizeT) where
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
type ProcCtx sizeT = Ctx.Context (ProcDef sizeT)

class HasProcCtx s where
  _procCtx :: (sizeT ~ SizeType s, precT ~ PrecType s) => Lens' s (ProcCtx sizeT)

instance HasProcCtx (ProcCtx sizeT) where _procCtx = id

-- | CQ Program
newtype Program sizeT = Program [ProcDef sizeT]
  deriving (Eq, Show, Read)

type instance SizeType (Program sizeT) = sizeT

instance (Show sizeT) => PP.ToCodeString (Program sizeT) where
  build (Program ps) = do
    mapM_ (PP.build >=> const PP.endl) ps

-- ================================================================================
-- Lenses
-- ================================================================================

instance HasAst (Stmt sizeT) where
  _ast focus (SeqS ss) = SeqS <$> traverse focus ss
  _ast focus (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> focus s_true <*> focus s_false
  _ast focus (RepeatS n_iter loop_body) = RepeatS n_iter <$> focus loop_body
  _ast _ s = pure s

instance HasStmt (Stmt sizeT) where
  type StmtOf (Stmt sizeT) = Stmt sizeT
  _stmt = id

instance HasStmt (CProcBody sizeT) where
  type StmtOf (CProcBody sizeT) = Stmt sizeT
  _stmt focus b@CProcBody{cproc_body_stmt} = focus cproc_body_stmt <&> \s' -> b{cproc_body_stmt = s'}
  _stmt _ b@CProcDecl{} = pure b

instance HasStmt (ProcDef sizeT) where
  type StmtOf (ProcDef sizeT) = Stmt sizeT
  _stmt focus p@ProcDef{proc_body = ProcBodyC b} =
    _stmt focus b <&> \b' -> p{proc_body = ProcBodyC b'}
  _stmt _ p = pure p

instance HasStmt (Program sizeT) where
  type StmtOf (Program sizeT) = Stmt sizeT
  _stmt focus (Program ps) = Program <$> traverse (_stmt focus) ps

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
  Stmt sizeT ->
  Stmt sizeT
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
  Stmt sizeT ->
  Stmt sizeT
whileKWithCondExpr k cond_var cond_expr body =
  SeqS
    [ compute_cond
    , whileK k cond_var (SeqS [body, compute_cond])
    ]
 where
  compute_cond = AssignS [cond_var] cond_expr

forInArray :: Ident -> VarType sizeT -> [P.BasicExpr sizeT] -> Stmt sizeT -> Stmt sizeT
forInArray i _ty ix_vals s =
  SeqS
    [ SeqS [AssignS{rets = [i], expr = v}, s]
    | v <- ix_vals
    ]

-- | Remove syntax sugar. Use with `rewriteAST`.
class CanDesugar p where
  desugarS :: p -> Maybe p

instance CanDesugar (Stmt sizeT) where
  desugarS WhileK{n_iter, cond, loop_body} = Just $ whileK n_iter cond loop_body
  desugarS WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = Just $ whileKWithCondExpr n_iter cond cond_expr loop_body
  desugarS ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = Just $ forInArray loop_index loop_index_ty loop_values loop_body
  desugarS _ = Nothing

instance CanDesugar (UStmt sizeT) where
  desugarS UWithComputedS{with_ustmt, body_ustmt} = Just $ USeqS [with_ustmt, body_ustmt, adjoint with_ustmt]
  desugarS _ = Nothing

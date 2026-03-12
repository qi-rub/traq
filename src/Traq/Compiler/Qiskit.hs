{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use camelCase" -}

module Traq.Compiler.Qiskit (
  toPy,
) where

import Control.Monad.Reader (Reader, runReader)
import Data.List (intersperse)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Lens.Micro.Mtl

import qualified Traq.CPL as CPL
import Traq.Compiler.Python
import Traq.Prelude
import qualified Traq.QPL as QPL

-- ============================================================
-- Compile QPL -> py (+Qiskit)
-- ============================================================

-- | Build python code string.
class ToQiskitPy a where
  type Ctx a

  mkPy :: a -> Reader (Ctx a) (Py ann)

-- | Convert a QPL program to a python code string.
toPy :: QPL.Program SizeT -> String
toPy prog =
  let pyDoc = runReader (mkPy prog) ()
   in show pyDoc

-- ============================================================
-- Basic Instances
-- ============================================================

instance (Show size, Integral size) => ToQiskitPy (QPL.Program size) where
  type Ctx (QPL.Program size) = ()

  mkPy (QPL.Program ps) =
    PP.vsep . intersperse PP.line <$> mapM mkPy ps

instance (Show size, Integral size) => ToQiskitPy (QPL.ProcDef size) where
  type Ctx (QPL.ProcDef size) = ()

  mkPy QPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} =
    PP.vsep
      <$> sequence
        [ pure $ py_comment info_comment
        , withEnv
            (ProcBuildCtx{..})
            (mkPy proc_body)
        ]

data ProcBuildCtx size = ProcBuildCtx
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [CPL.VarType size]
  }
  deriving (Read, Show, Eq)

instance (Show size, Integral size) => ToQiskitPy (QPL.ProcBody size) where
  type Ctx (QPL.ProcBody size) = ProcBuildCtx size

  mkPy (QPL.ProcBodyU ubody) = mkPy ubody
  mkPy (QPL.ProcBodyC cbody) = mkPy cbody

-- ============================================================
-- Unitary: Emit Qiskit unitary circuits
-- ============================================================

instance (Show size, Integral size) => ToQiskitPy (QPL.UProcBody size) where
  type Ctx (QPL.UProcBody size) = ProcBuildCtx size

  mkPy QPL.UProcDecl = do
    ProcBuildCtx{..} <- view id
    error "TODO UProcDecl"
  mkPy QPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    error "TODO UProcBody"

instance (Show size, Integral size) => ToQiskitPy (QPL.UStmt size) where
  type Ctx (QPL.UStmt size) = CPL.TypingCtx size

  mkPy QPL.USkipS = pure mempty
  mkPy (QPL.UCommentS s) = pure $ py_comment s
  mkPy QPL.UnitaryS{qargs, unitary} = error "TODO UnitaryS"
  mkPy QPL.UCallS{uproc_id, dagger, qargs} = error "TODO UCallS"
  mkPy (QPL.USeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy QPL.URepeatS{n_iter, uloop_body} = error "TODO URepeatS"
  mkPy QPL.UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = error "TODO UForInRangeS"
  mkPy QPL.UForInDomainS{iter_meta_var, iter_ty, dagger, uloop_body} = error "TODO UForInDomainS"
  mkPy QPL.UWithComputedS{with_ustmt, body_ustmt} = error "TODO UWithComputedS"

instance (Show size, Integral size) => ToQiskitPy (QPL.Unitary size) where
  type Ctx (QPL.Unitary size) = [CPL.VarType size]

  mkPy (QPL.BasicGateU g) = mkPy g
  mkPy (QPL.DistrU d) = error "TODO DistrU"
  mkPy (QPL.Controlled u) = error "TODO Controlled"
  mkPy (QPL.Adjoint u) = error "TODO Adjoint"
  mkPy (QPL.RevEmbedU xs e) = error "TODO RevEmbedU"

instance (Show size, Integral size) => ToQiskitPy (QPL.BasicGate size) where
  type Ctx (QPL.BasicGate size) = [CPL.VarType size]

  mkPy QPL.Toffoli = error "TODO Toffoli"
  mkPy QPL.CNOT = error "TODO CNOT"
  mkPy QPL.XGate = error "TODO XGate"
  mkPy QPL.HGate = error "TODO HGate"
  mkPy QPL.ZGate = error "TODO ZGate"
  mkPy (QPL.Rz theta) = error "TODO Rz"
  mkPy QPL.COPY = error "TODO COPY"
  mkPy QPL.SWAP = error "TODO SWAP"
  mkPy (QPL.PhaseOnZero theta) = error "TODO PhaseOnZero"

-- ============================================================
-- Classical: Emit Qiskit circuits with control-flow
-- ============================================================

instance (Show size) => ToQiskitPy (QPL.CProcBody size) where
  type Ctx (QPL.CProcBody size) = ProcBuildCtx size

  -- external
  mkPy QPL.CProcDecl = do
    ProcBuildCtx{..} <- view id
    let n_args = length proc_param_types
    let cproc_param_names = ["arg_" <> show i | i <- [1 .. n_args]]
    let tys = map toPyType proc_param_types

    let typed_args = map py_sanitizeIdent proc_meta_params ++ zipWith py_typedArg cproc_param_names tys
    pure $
      py_def proc_name typed_args $
        py_notImplemented "external function - implement here"

  -- defined
  mkPy QPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let tys = map toPyType proc_param_types

    let untyped_args = map py_sanitizeIdent (proc_meta_params ++ cproc_param_names)
    let typed_args = map py_sanitizeIdent proc_meta_params ++ zipWith py_typedArg cproc_param_names tys

    py_def proc_name typed_args . PP.vsep <$> do
      body <- withEnv () $ mkPy cproc_body_stmt
      return
        [ body
        , py_return untyped_args
        ]

instance (Show size) => ToQiskitPy (QPL.Stmt size) where
  type Ctx (QPL.Stmt size) = ()

  mkPy QPL.SkipS = pure py_pass
  mkPy (QPL.CommentS s) = pure $ py_comment s
  mkPy QPL.AssignS{rets, expr} = do
    let lhs = PP.hsep $ PP.punctuate PP.comma (map py_sanitizeIdent rets)
    pure $ lhs <+> PP.equals <+> py_expr expr
  mkPy QPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy QPL.RandomDynS{ret, max_var} =
    pure $ PP.pretty ret <+> PP.equals <+> PP.pretty "random.randrange" <> PP.parens (PP.pretty max_var)
  mkPy QPL.CallS{fun = QPL.FunctionCall proc_id, meta_params, args} = do
    let fname = py_sanitizeIdent proc_id
    let py_mps = map py_metaParam meta_params
    let py_args = map py_arg args
    let all_args = py_mps ++ py_args
    let arg_vars = map py_arg args
    let lhs = PP.hsep $ PP.punctuate PP.comma arg_vars
    pure $ lhs <+> PP.equals <+> fname <> PP.tupled all_args
  mkPy QPL.CallS{fun = QPL.UProcAndMeas proc_id, meta_params, args} = error "TODO UProcAndMeas"
  mkPy (QPL.SeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy QPL.IfThenElseS{cond, s_true, s_false} = py_ifte cond <$> mkPy s_true <*> mkPy s_false
  mkPy QPL.RepeatS{n_iter, loop_body} = do
    body <- mkPy loop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "for _ in range" <> PP.parens n <> PP.colon
        , py_indent body
        ]
  mkPy QPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy QPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy QPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = do
    body <- mkPy loop_body
    let vals = PP.list (map py_expr loop_values)
    pure $
      PP.vsep
        [ PP.pretty "for" <+> PP.pretty loop_index <+> PP.pretty "in" <+> vals <> PP.colon
        , py_indent body
        ]
  mkPy QPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"

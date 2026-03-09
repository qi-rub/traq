{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Traq.Compiler.Qualtran (
  toPy,
) where

import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Data.List (intersperse)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.CQPL as CQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- ============================================================
-- Compile QPL -> py (+Qualtran)
-- ============================================================

type Py ann = PP.Doc ann

-- | Build python code string.
class ToQualtranPy a where
  type Ctx a

  mkPy :: a -> Reader (Ctx a) (Py ann)

-- | Convert a CQPL program to a python code string.
toPy :: CQPL.Program SizeT -> String
toPy prog =
  let pyDoc = runReader (mkPy prog) ()
   in show pyDoc

-- ============================================================
-- Helpers for building python syntax
-- ============================================================

withEnv :: (Monad m) => r -> ReaderT r m a -> ReaderT r' m a
withEnv r = magnify (lens (const r) const)

py_indent :: Py ann -> Py ann
py_indent = PP.indent tabwidth
 where
  tabwidth :: Int
  tabwidth = 4

-- ------------------------------------------------------------
-- variables and types
-- ------------------------------------------------------------
py_typed_arg :: String -> Py ann -> Py ann
py_typed_arg x t = PP.pretty x <+> PP.colon <+> t

-- ------------------------------------------------------------
-- statements
-- ------------------------------------------------------------

py_comment :: String -> Py ann
py_comment c = PP.vsep $ lines c <&> \l -> PP.pretty $ "# " <> l

py_pass :: Py ann
py_pass = PP.pretty "pass"

py_ifte :: String -> Py ann -> Py ann -> Py ann
py_ifte b s_t s_f =
  PP.vsep
    [ PP.pretty $ "if (" <> b <> "):"
    , py_indent s_t
    , PP.pretty "else:"
    , py_indent s_f
    ]

py_raise_s :: String -> Py ann
py_raise_s e = PP.pretty @String $ printf "raise Exception('%s')" e

py_def :: Ident -> [Py ann] -> Py ann -> Py ann
py_def name params body =
  PP.vsep
    [ PP.pretty "def" <+> PP.pretty name <> PP.tupled params <> PP.pretty ":"
    , py_indent body
    ]

py_class :: Ident -> Py ann -> Py ann
py_class = undefined

-- ============================================================
-- Basic Instances
-- ============================================================

instance ToQualtranPy (CQPL.Program size) where
  type Ctx (CQPL.Program size) = ()

  mkPy (CQPL.Program ps) =
    PP.vsep . intersperse PP.line <$> mapM mkPy ps

instance ToQualtranPy (CQPL.ProcDef size) where
  type Ctx (CQPL.ProcDef size) = ()

  mkPy CQPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} =
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
  , proc_param_types :: [P.VarType size]
  }
  deriving (Read, Show, Eq)

instance ToQualtranPy (CQPL.ProcBody size) where
  type Ctx (CQPL.ProcBody size) = ProcBuildCtx size

  mkPy (CQPL.ProcBodyU ubody) = mkPy ubody
  mkPy (CQPL.ProcBodyC cbody) = mkPy cbody

-- ============================================================
-- Unitary: Emit Qualtran Bloqs
-- ============================================================

instance ToQualtranPy (CQPL.UProcBody size) where
  type Ctx (CQPL.UProcBody size) = ProcBuildCtx size

  mkPy CQPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = do
    pure $ py_raise_s "TODO UProcBody"
  mkPy CQPL.UProcDecl =
    pure $ py_raise_s "TODO UProcDecl"

instance ToQualtranPy (CQPL.UStmt size) where
  type Ctx (CQPL.UStmt size) = ()

  mkPy CQPL.USkipS = error "TODO USkipS"
  mkPy CQPL.UnitaryS{qargs, unitary} = error "TODO UnitaryS"
  mkPy CQPL.UCallS{uproc_id, dagger, qargs} = error "TODO UCallS"
  mkPy (CQPL.USeqS ss) = error "TODO USeqS"
  mkPy (CQPL.UCommentS s) = error "TODO UCommentS"
  mkPy CQPL.URepeatS{n_iter, uloop_body} = error "TODO URepeatS"
  mkPy CQPL.UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = error "TODO UForInRangeS"
  mkPy CQPL.UForInDomainS{iter_meta_var, iter_ty, dagger, uloop_body} = error "TODO UForInDomainS"
  mkPy CQPL.UWithComputedS{with_ustmt, body_ustmt} = error "TODO UWithComputedS"

instance ToQualtranPy (CQPL.Unitary size) where
  type Ctx (CQPL.Unitary size) = ()

  mkPy (CQPL.BasicGateU g) = error "TODO BasicGateU"
  mkPy (CQPL.RevEmbedU xs e) = error "TODO RevEmbedU"
  mkPy (CQPL.DistrU d) = error "TODO DistrU"
  mkPy (CQPL.Controlled u) = error "TODO Controlled"
  mkPy (CQPL.Adjoint u) = error "TODO Adjoint"

instance ToQualtranPy CQPL.BasicGate where
  type Ctx CQPL.BasicGate = ()

  mkPy CQPL.Toffoli = error "TODO Toffoli"
  mkPy CQPL.CNOT = error "TODO CNOT"
  mkPy CQPL.XGate = error "TODO XGate"
  mkPy CQPL.HGate = error "TODO HGate"
  mkPy CQPL.ZGate = error "TODO ZGate"
  mkPy CQPL.COPY = error "TODO COPY"
  mkPy CQPL.SWAP = error "TODO SWAP"
  mkPy (CQPL.Rz theta) = error "TODO Rz"
  mkPy (CQPL.PhaseOnZero theta) = error "TODO PhaseOnZero"

-- ============================================================
-- Classical: Emit native python
-- ============================================================

toPyType :: P.VarType size -> Py ann
toPyType (P.Fin _) = PP.pretty "int"
toPyType (P.Bitvec _) = PP.pretty "int"
toPyType (P.Tup ts) = PP.pretty "tuple" <+> PP.brackets (PP.tupled (map toPyType ts))
toPyType (P.Arr _ t) = PP.pretty "list" <+> PP.brackets (toPyType t)

instance ToQualtranPy (CQPL.CProcBody size) where
  type Ctx (CQPL.CProcBody size) = ProcBuildCtx size

  -- external
  mkPy CQPL.CProcDecl = do
    ProcBuildCtx{..} <- view id
    let n_args = length proc_param_types
    let cproc_param_names = ["arg_" <> show i | i <- [1 .. n_args]]
    let tys = map toPyType proc_param_types

    let typed_args = map PP.pretty proc_meta_params ++ zipWith py_typed_arg cproc_param_names tys
    pure $
      py_def proc_name typed_args $
        py_raise_s "external function - implement here"

  -- defined
  mkPy CQPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let tys = map toPyType proc_param_types

    let typed_args = map PP.pretty proc_meta_params ++ zipWith py_typed_arg cproc_param_names tys

    py_def proc_name typed_args <$> do
      withEnv () $ mkPy cproc_body_stmt

instance ToQualtranPy (CQPL.Stmt size) where
  type Ctx (CQPL.Stmt size) = ()

  mkPy CQPL.SkipS = pure py_pass
  mkPy (CQPL.CommentS s) = pure $ py_comment s
  mkPy CQPL.AssignS{rets, expr} = error "TODO AssignS"
  mkPy CQPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy CQPL.RandomDynS{ret, max_var} = error "TODO RandomDynS"
  mkPy CQPL.CallS{fun, meta_params, args} = error "TODO CallS"
  mkPy (CQPL.SeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy CQPL.IfThenElseS{cond, s_true, s_false} = py_ifte cond <$> mkPy s_true <*> mkPy s_false
  mkPy CQPL.RepeatS{n_iter, loop_body} = error "TODO RepeatS"
  mkPy CQPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy CQPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy CQPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = error "TODO ForInArray"
  mkPy CQPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"

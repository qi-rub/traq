{-# LANGUAGE DuplicateRecordFields #-}

module Traq.Compiler.Qualtran (
  toPy,
) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, execWriterT)
import Text.Read (readMaybe)

import qualified Traq.CQPL as CQPL
import Traq.Prelude
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- Compile QPL -> py (+Qualtran)
-- ============================================================

type PyBuilder m = (MonadWriter [String] m, MonadFail m)

-- | Build python code string.
class ToQualtranPy a where
  mkPy :: (PyBuilder m) => a -> m ()

-- | Convert a CQPL program to a python code string.
toPy :: (MonadFail m) => CQPL.Program SizeT -> m String
toPy = fmap unlines . execWriterT . mkPy

-- ============================================================
-- Helpers for building python syntax
-- ============================================================

pyClass :: (PyBuilder m) => Ident -> m () -> m ()
pyClass = undefined

-- ============================================================
-- Instances
-- ============================================================

instance ToQualtranPy (CQPL.Program size) where
  mkPy (CQPL.Program ps) = mapM_ (\p -> mkPy p >> PP.endl) ps

instance ToQualtranPy (CQPL.ProcDef size) where
  mkPy CQPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} = error "TODO ProcDef"

instance ToQualtranPy (CQPL.ProcBody size) where
  mkPy (CQPL.ProcBodyU ubody) = error "TODO ProcBodyU"
  mkPy (CQPL.ProcBodyC cbody) = error "TODO ProcBodyC"

instance ToQualtranPy (CQPL.UProcBody size) where
  mkPy CQPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = error "TODO UProcBody"
  mkPy CQPL.UProcDecl = error "TODO UProcDecl"

instance ToQualtranPy (CQPL.CProcBody size) where
  mkPy CQPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = error "TODO CProcBody"
  mkPy CQPL.CProcDecl = error "TODO CProcDecl"

instance ToQualtranPy CQPL.ParamTag where
  mkPy CQPL.ParamCtrl = error "TODO ParamCtrl"
  mkPy CQPL.ParamInp = error "TODO ParamInp"
  mkPy CQPL.ParamOut = error "TODO ParamOut"
  mkPy CQPL.ParamAux = error "TODO ParamAux"
  mkPy CQPL.ParamUnk = error "TODO ParamUnk"

instance ToQualtranPy (CQPL.UStmt size) where
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
  mkPy (CQPL.BasicGateU g) = error "TODO BasicGateU"
  mkPy (CQPL.RevEmbedU xs e) = error "TODO RevEmbedU"
  mkPy (CQPL.DistrU d) = error "TODO DistrU"
  mkPy (CQPL.Controlled u) = error "TODO Controlled"
  mkPy (CQPL.Adjoint u) = error "TODO Adjoint"

instance ToQualtranPy CQPL.BasicGate where
  mkPy CQPL.Toffoli = error "TODO Toffoli"
  mkPy CQPL.CNOT = error "TODO CNOT"
  mkPy CQPL.XGate = error "TODO XGate"
  mkPy CQPL.HGate = error "TODO HGate"
  mkPy CQPL.ZGate = error "TODO ZGate"
  mkPy CQPL.COPY = error "TODO COPY"
  mkPy CQPL.SWAP = error "TODO SWAP"
  mkPy (CQPL.Rz theta) = error "TODO Rz"
  mkPy (CQPL.PhaseOnZero theta) = error "TODO PhaseOnZero"

instance ToQualtranPy (CQPL.Stmt size) where
  mkPy CQPL.SkipS = error "TODO SkipS"
  mkPy (CQPL.CommentS s) = error "TODO CommentS"
  mkPy CQPL.AssignS{rets, expr} = error "TODO AssignS"
  mkPy CQPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy CQPL.RandomDynS{ret, max_var} = error "TODO RandomDynS"
  mkPy CQPL.CallS{fun, meta_params, args} = error "TODO CallS"
  mkPy (CQPL.SeqS ss) = error "TODO SeqS"
  mkPy CQPL.IfThenElseS{cond, s_true, s_false} = error "TODO IfThenElseS"
  mkPy CQPL.RepeatS{n_iter, loop_body} = error "TODO RepeatS"
  mkPy CQPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy CQPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy CQPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = error "TODO ForInArray"
  mkPy CQPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"

instance ToQualtranPy CQPL.FunctionCall where
  mkPy (CQPL.FunctionCall name) = error "TODO FunctionCall"
  mkPy (CQPL.UProcAndMeas name) = error "TODO UProcAndMeas"

instance ToQualtranPy (CQPL.Arg size) where
  mkPy (CQPL.Arg x) = error "TODO Arg"
  mkPy (CQPL.ArrElemArg arg i) = error "TODO ArrElemArg"

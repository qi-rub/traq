{-# LANGUAGE DuplicateRecordFields #-}

module Traq.Compiler.Qualtran (
  toPy,
) where

import Control.Monad (unless, when)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, execWriterT)
import Text.Read (readMaybe)

import qualified Traq.CQPL as CQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ============================================================
-- Compile QPL -> py (+Qualtran)
-- ============================================================

type PyBuilder m = (MonadWriter [String] m, MonadFail m)

-- | Build python code string.
class ToQualtranPy a where
  type Ctx a

  mkPy :: (PyBuilder m) => Ctx a -> a -> m ()

-- | Convert a CQPL program to a python code string.
toPy :: (MonadFail m) => CQPL.Program SizeT -> m String
toPy = fmap unlines . execWriterT . mkPy ()

-- ============================================================
-- Helpers for building python syntax
-- ============================================================

pyClass :: (PyBuilder m) => Ident -> m () -> m ()
pyClass = undefined

-- ============================================================
-- Instances
-- ============================================================

instance ToQualtranPy (CQPL.Program size) where
  type Ctx (CQPL.Program size) = ()

  mkPy _ (CQPL.Program ps) = mapM_ (\p -> mkPy () p >> PP.endl) ps

instance ToQualtranPy (CQPL.ProcDef size) where
  type Ctx (CQPL.ProcDef size) = ()

  mkPy _ CQPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} = do
    PP.putComment info_comment
    mkPy (proc_name, proc_meta_params, proc_param_types) proc_body

instance ToQualtranPy (CQPL.ProcBody size) where
  type Ctx (CQPL.ProcBody size) = (Ident, [Ident], [P.VarType size])

  mkPy ctx (CQPL.ProcBodyU ubody) = mkPy ctx ubody
  mkPy ctx (CQPL.ProcBodyC cbody) = mkPy ctx cbody

instance ToQualtranPy (CQPL.UProcBody size) where
  type Ctx (CQPL.UProcBody size) = (Ident, [Ident], [P.VarType size])

  mkPy _ctx CQPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = error "TODO UProcBody"
  mkPy _ctx CQPL.UProcDecl = error "TODO UProcDecl"

instance ToQualtranPy (CQPL.CProcBody size) where
  type Ctx (CQPL.CProcBody size) = (Ident, [Ident], [P.VarType size])

  mkPy _ctx CQPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = error "TODO CProcBody"
  mkPy _ctx CQPL.CProcDecl = error "TODO CProcDecl"

instance ToQualtranPy (CQPL.UStmt size) where
  type Ctx (CQPL.UStmt size) = ()

  mkPy _ CQPL.USkipS = error "TODO USkipS"
  mkPy _ CQPL.UnitaryS{qargs, unitary} = error "TODO UnitaryS"
  mkPy _ CQPL.UCallS{uproc_id, dagger, qargs} = error "TODO UCallS"
  mkPy _ (CQPL.USeqS ss) = error "TODO USeqS"
  mkPy _ (CQPL.UCommentS s) = error "TODO UCommentS"
  mkPy _ CQPL.URepeatS{n_iter, uloop_body} = error "TODO URepeatS"
  mkPy _ CQPL.UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = error "TODO UForInRangeS"
  mkPy _ CQPL.UForInDomainS{iter_meta_var, iter_ty, dagger, uloop_body} = error "TODO UForInDomainS"
  mkPy _ CQPL.UWithComputedS{with_ustmt, body_ustmt} = error "TODO UWithComputedS"

instance ToQualtranPy (CQPL.Unitary size) where
  type Ctx (CQPL.Unitary size) = ()

  mkPy _ (CQPL.BasicGateU g) = error "TODO BasicGateU"
  mkPy _ (CQPL.RevEmbedU xs e) = error "TODO RevEmbedU"
  mkPy _ (CQPL.DistrU d) = error "TODO DistrU"
  mkPy _ (CQPL.Controlled u) = error "TODO Controlled"
  mkPy _ (CQPL.Adjoint u) = error "TODO Adjoint"

instance ToQualtranPy CQPL.BasicGate where
  type Ctx CQPL.BasicGate = ()

  mkPy _ CQPL.Toffoli = error "TODO Toffoli"
  mkPy _ CQPL.CNOT = error "TODO CNOT"
  mkPy _ CQPL.XGate = error "TODO XGate"
  mkPy _ CQPL.HGate = error "TODO HGate"
  mkPy _ CQPL.ZGate = error "TODO ZGate"
  mkPy _ CQPL.COPY = error "TODO COPY"
  mkPy _ CQPL.SWAP = error "TODO SWAP"
  mkPy _ (CQPL.Rz theta) = error "TODO Rz"
  mkPy _ (CQPL.PhaseOnZero theta) = error "TODO PhaseOnZero"

instance ToQualtranPy (CQPL.Stmt size) where
  type Ctx (CQPL.Stmt size) = ()

  mkPy _ CQPL.SkipS = error "TODO SkipS"
  mkPy _ (CQPL.CommentS s) = error "TODO CommentS"
  mkPy _ CQPL.AssignS{rets, expr} = error "TODO AssignS"
  mkPy _ CQPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy _ CQPL.RandomDynS{ret, max_var} = error "TODO RandomDynS"
  mkPy _ CQPL.CallS{fun, meta_params, args} = error "TODO CallS"
  mkPy _ (CQPL.SeqS ss) = error "TODO SeqS"
  mkPy _ CQPL.IfThenElseS{cond, s_true, s_false} = error "TODO IfThenElseS"
  mkPy _ CQPL.RepeatS{n_iter, loop_body} = error "TODO RepeatS"
  mkPy _ CQPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy _ CQPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy _ CQPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = error "TODO ForInArray"
  mkPy _ CQPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"

instance ToQualtranPy (CQPL.Arg size) where
  type Ctx (CQPL.Arg size) = ()

  mkPy _ (CQPL.Arg x) = PP.putWord x
  mkPy _ (CQPL.ArrElemArg arg i) = error "TODO ArrElemArg"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Error.Unitary (
  TraceNormErrorU (..),
  traceNormErrorUProg,
) where

import Control.Monad.Reader (runReader)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad (non', (??))
import qualified Traq.Data.Context as Ctx

import Traq.Analysis.Error.Prelude
import Traq.Prelude
import Traq.ProtoLang

-- | Trace-norm error w.r.t unitary compiler
class
  ( ErrorReqs size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  TraceNormErrorU ext size prec
    | ext -> size prec
  where
  traceNormErrorU ::
    forall ext' m.
    ( m ~ ErrorAnalysisMonad ext'
    , TraceNormErrorU ext' size prec
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    ) =>
    ext ->
    m (FailProb prec)

-- ================================================================================
-- Core Language
-- ================================================================================

instance (TraceNormErrorU ext size prec) => TraceNormErrorU (Expr ext) size prec where
  traceNormErrorU BasicExprE{} = return 0
  traceNormErrorU RandomSampleE{} = return 0
  traceNormErrorU FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    traceNormErrorU fn
  traceNormErrorU PrimCallE{prim} = traceNormErrorU prim
  traceNormErrorU _ = error "unsupported"

instance (TraceNormErrorU ext size prec) => TraceNormErrorU (Stmt ext) size prec where
  traceNormErrorU ExprS{expr} = traceNormErrorU expr
  traceNormErrorU IfThenElseS{s_true, s_false} = do
    eps_t <- traceNormErrorU s_true
    eps_f <- traceNormErrorU s_false
    return $ eps_t + eps_f
  traceNormErrorU (SeqS ss) = sum <$> mapM traceNormErrorU ss

instance (TraceNormErrorU ext size prec) => TraceNormErrorU (FunDef ext) size prec where
  traceNormErrorU FunDef{mbody = Nothing} = return 0
  traceNormErrorU FunDef{mbody = Just FunBody{body_stmt}} = traceNormErrorU body_stmt

instance (TraceNormErrorU ext size prec) => TraceNormErrorU (NamedFunDef ext) size prec where
  traceNormErrorU = traceNormErrorU . fun_def

instance (ErrorReqs size prec) => TraceNormErrorU (Core size prec) size prec where
  traceNormErrorU = \case {}

-- ================================================================================
-- Entry Points
-- ================================================================================

traceNormErrorUProg ::
  ( TraceNormErrorU ext size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  Program ext ->
  FailProb prec
traceNormErrorUProg (Program fs) =
  traceNormErrorU main_fn & runReader ?? (namedFunsToFunCtx fs)
 where
  main_fn = fun_def $ last fs

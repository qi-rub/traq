{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Error (Failure Probability) analysis for the quantum compiler.
module Traq.Analysis.Error.Quantum (
  TVErrorQ (..),
  tvErrorQProg,
) where

import Control.Monad.Reader (runReader)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Analysis.Error.Prelude
import Traq.Analysis.Error.Unitary (TraceNormErrorU)
import Traq.Prelude
import Traq.ProtoLang

-- | Total variation distance w.r.t. quantum compiler CompileQ.
class
  ( ErrorReqs size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  , TraceNormErrorU ext size prec
  ) =>
  TVErrorQ ext size prec
    | ext -> size prec
  where
  tvErrorQ ::
    forall ext' m.
    ( m ~ ErrorAnalysisMonad ext'
    , TVErrorQ ext' size prec
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    ) =>
    ext ->
    m (FailProb prec)

-- ================================================================================
-- Core Language
-- ================================================================================

instance (TVErrorQ ext size prec) => TVErrorQ (Expr ext) size prec where
  tvErrorQ BasicExprE{} = return 0
  tvErrorQ RandomSampleE{} = return 0
  tvErrorQ FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    tvErrorQ fn
  tvErrorQ PrimCallE{prim} = tvErrorQ prim
  tvErrorQ _ = error "unsupported"

instance (TVErrorQ ext size prec) => TVErrorQ (Stmt ext) size prec where
  tvErrorQ ExprS{expr} = tvErrorQ expr
  tvErrorQ IfThenElseS{s_true, s_false} = do
    eps_t <- tvErrorQ s_true
    eps_f <- tvErrorQ s_false
    return $ max eps_t eps_f
  tvErrorQ (SeqS ss) = sum <$> mapM tvErrorQ ss

instance (TVErrorQ ext size prec) => TVErrorQ (FunDef ext) size prec where
  tvErrorQ FunDef{mbody = Nothing} = return 0
  tvErrorQ FunDef{mbody = Just FunBody{body_stmt}} = tvErrorQ body_stmt

instance (ErrorReqs size prec) => TVErrorQ (Core size prec) size prec where
  tvErrorQ = \case {}

-- ================================================================================
-- Entry Points
-- ================================================================================

tvErrorQProg ::
  ( TVErrorQ ext size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  Program ext ->
  FailProb prec
tvErrorQProg (Program fs) =
  tvErrorQ main_fn & runReader ?? (namedFunsToFunCtx fs)
 where
  main_fn = fun_def $ last fs

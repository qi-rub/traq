{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Error.Unitary (
  TVErrorU (..),
  tvErrorUProg,
) where

import Control.Monad.Reader (runReader)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad (non', (??))
import qualified Traq.Data.Context as Ctx

import Traq.Analysis.Error.Prelude
import Traq.Analysis.Prelude (sizeToPrec)
import Traq.CPL
import Traq.Prelude

-- | Total variation error w.r.t unitary compiler
class
  ( ErrorReqs size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  TVErrorU ext size prec
    | ext -> size prec
  where
  tvErrorU ::
    forall ext' m.
    ( m ~ ErrorAnalysisMonad ext'
    , TVErrorU ext' size prec
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    ) =>
    ext ->
    m (FailProb prec)

-- ================================================================================
-- Core Language
-- ================================================================================

instance (TVErrorU ext size prec) => TVErrorU (Expr ext) size prec where
  tvErrorU BasicExprE{} = return 0
  tvErrorU RandomSampleE{} = return 0
  tvErrorU FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    tvErrorU fn
  tvErrorU PrimCallE{prim} = tvErrorU prim

instance (TVErrorU ext size prec) => TVErrorU (Stmt ext) size prec where
  tvErrorU ExprS{expr} = tvErrorU expr
  tvErrorU IfThenElseS{s_true, s_false} = do
    eps_t <- tvErrorU s_true
    eps_f <- tvErrorU s_false
    return $ eps_t + eps_f
  tvErrorU (SeqS ss) = sum <$> mapM tvErrorU ss
  tvErrorU ForS{loop_ty, loop_body} = do
    body_err <- tvErrorU loop_body
    let n_iters = loop_ty ^?! _Fin
    return $ failProb (sizeToPrec n_iters) * body_err

instance (TVErrorU ext size prec) => TVErrorU (FunDef ext) size prec where
  tvErrorU FunDef{mbody = Nothing} = return 0
  tvErrorU FunDef{mbody = Just FunBody{body_stmt}} = tvErrorU body_stmt

instance (TVErrorU ext size prec) => TVErrorU (NamedFunDef ext) size prec where
  tvErrorU = tvErrorU . fun_def

instance (ErrorReqs size prec) => TVErrorU (Core size prec) size prec where
  tvErrorU = \case {}

-- ================================================================================
-- Entry Points
-- ================================================================================

tvErrorUProg ::
  ( TVErrorU ext size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  Program ext ->
  FailProb prec
tvErrorUProg (Program fs) =
  tvErrorU main_fn & runReader ?? namedFunsToFunCtx fs
 where
  main_fn = fun_def $ last fs

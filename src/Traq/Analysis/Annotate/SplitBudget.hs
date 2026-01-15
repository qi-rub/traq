{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Traq.Analysis.Annotate.SplitBudget (
  AnnotateWithErrorBudgetU (..),
  AnnotateWithErrorBudgetQ (..),
  annEpsU1,
  annEpsQ1,
  canError,
  annotateProgWithErrorBudget,
  annotateProgWithErrorBudgetU,
) where

import Control.Monad.RWS (runRWST)
import Control.Monad.State (MonadState)
import Data.Kind (Type)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default (default_)

import Traq.Analysis.Annotate.Basic
import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Error.Prelude
import Traq.Prelude
import Traq.ProtoLang

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Predicate for checking if an expr/stmt/prog can fail
class CanError p where
  type ExtensionType p

  canError ::
    ( ext ~ ExtensionType p
    , MonadState st m
    , HasFunCtx st ext
    ) =>
    p ->
    m Bool

instance CanError (Expr ext) where
  type ExtensionType (Expr ext) = ext

  canError FunCallE{fname} = do
    use (_funCtx . Ctx.at fname) >>= maybe (return False) canError
  canError PrimCallE{} = return True
  canError _ = return False

instance CanError (Stmt ext) where
  type ExtensionType (Stmt ext) = ext

  canError ExprS{expr} = canError expr
  canError (SeqS ss) = or <$> mapM canError ss
  canError IfThenElseS{s_true, s_false} = (||) <$> canError s_true <*> canError s_false

instance CanError (FunDef ext) where
  type ExtensionType (FunDef ext) = ext

  canError FunDef{mbody} = case mbody of
    Nothing -> return False
    Just FunBody{body_stmt} -> canError body_stmt

-- ============================================================================
-- Annotation
-- ============================================================================

-- | Update the annotation given a total error budget for this primitive call.
class AnnotateWithErrorBudgetU ext where
  annEpsU ::
    forall ext' size prec m.
    ( ext' ~ AnnFailProb ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , ErrorReqs size prec
    , m ~ AnnotateMonad ext ext'
    ) =>
    FailProb prec ->
    ext' ->
    m ext'

class AnnotateWithErrorBudgetQ ext where
  annEpsQ ::
    forall ext' size prec m.
    ( ext' ~ AnnFailProb ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , ErrorReqs size prec
    , m ~ AnnotateMonad ext ext'
    ) =>
    FailProb prec ->
    ext' ->
    m ext'

-- | Internal class for unitary error budget
class AnnotateWithErrorBudgetU1 (p :: Type -> Type) where
  annEpsU1 ::
    forall ext ext' size prec m.
    ( AnnotateWithErrorBudgetU ext
    , ext' ~ AnnFailProb ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , ErrorReqs size prec
    , m ~ AnnotateMonad ext ext'
    ) =>
    FailProb prec ->
    p ext' ->
    m (p ext')

-- | Internal class for quantum error budget
class AnnotateWithErrorBudgetQ1 (p :: Type -> Type) where
  annEpsQ1 ::
    forall ext ext' size prec m.
    ( AnnotateWithErrorBudgetQ ext
    , ext' ~ AnnFailProb ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , ErrorReqs size prec
    , m ~ AnnotateMonad ext ext'
    ) =>
    FailProb prec ->
    p ext' ->
    m (p ext')

instance AnnotateWithErrorBudgetU1 Expr where
  annEpsU1 _ BasicExprE{..} = pure BasicExprE{..}
  annEpsU1 _ RandomSampleE{..} = pure RandomSampleE{..}
  annEpsU1 eps FunCallE{..} = do
    fn <- use (_funCtx . Ctx.at fname) >>= maybeWithError "cannot find function"
    annEpsU1 eps (NamedFunDef fname fn)
    pure FunCallE{..}
  annEpsU1 eps (PrimCallE ext') = PrimCallE <$> annEpsU eps ext'
  annEpsU1 _ _ = error "UNSUPPORTED"

instance AnnotateWithErrorBudgetQ1 Expr where
  annEpsQ1 _ BasicExprE{..} = pure BasicExprE{..}
  annEpsQ1 _ RandomSampleE{..} = pure RandomSampleE{..}
  annEpsQ1 eps FunCallE{..} = do
    fn <- use (_funCtx . Ctx.at fname) >>= maybeWithError "cannot find function"
    annEpsQ1 eps (NamedFunDef fname fn)
    pure FunCallE{..}
  annEpsQ1 eps (PrimCallE ext) = PrimCallE <$> annEpsQ eps ext
  annEpsQ1 _ _ = error "UNSUPPORTED"

instance AnnotateWithErrorBudgetU1 Stmt where
  annEpsU1 eps ExprS{rets, expr} = do
    expr <- annEpsU1 eps expr
    pure ExprS{rets, expr}
  annEpsU1 eps (SeqS ss) = do
    needs_eps <- mapM canError ss
    let k = length $ filter id needs_eps
    let eps' = if k == 0 then eps else splitFailProb eps (fromIntegral k)
    SeqS <$> mapM (annEpsU1 eps') ss
  annEpsU1 _ _ = error "UNSUPPORTED"

instance AnnotateWithErrorBudgetQ1 Stmt where
  annEpsQ1 eps ExprS{rets, expr} = do
    expr <- annEpsQ1 eps expr
    pure ExprS{rets, expr}
  annEpsQ1 eps (SeqS ss) = do
    needs_eps <- mapM canError ss
    let k = length $ filter id needs_eps
    if k == 0
      then pure (SeqS ss)
      else do
        let eps' = splitFailProb eps (fromIntegral k)
        SeqS <$> mapM (annEpsQ1 eps') ss
  annEpsQ1 _ _ = error "UNSUPPORTED"

instance AnnotateWithErrorBudgetU1 FunBody where
  annEpsU1 eps FunBody{..} = do
    body_stmt <- annEpsU1 eps body_stmt
    pure FunBody{..}

instance AnnotateWithErrorBudgetQ1 FunBody where
  annEpsQ1 eps FunBody{..} = do
    body_stmt <- annEpsQ1 eps body_stmt
    pure FunBody{..}

instance AnnotateWithErrorBudgetU1 FunDef where
  annEpsU1 _ FunDef{mbody = Nothing, ..} = pure $ FunDef{mbody = Nothing, ..}
  annEpsU1 eps FunDef{mbody = Just body, ..} = do
    body <- annEpsU1 eps body
    pure FunDef{mbody = Just body, ..}

instance AnnotateWithErrorBudgetQ1 FunDef where
  annEpsQ1 _ FunDef{mbody = Nothing, ..} = pure $ FunDef{mbody = Nothing, ..}
  annEpsQ1 eps FunDef{mbody = Just body, ..} = do
    body <- annEpsQ1 eps body
    pure FunDef{mbody = Just body, ..}

instance AnnotateWithErrorBudgetU1 NamedFunDef where
  annEpsU1 eps NamedFunDef{..} = do
    fun_def <- annEpsU1 eps fun_def
    _funCtx . Ctx.ix fun_name .= fun_def
    pure NamedFunDef{..}

instance AnnotateWithErrorBudgetQ1 NamedFunDef where
  annEpsQ1 eps NamedFunDef{..} = do
    fun_def <- annEpsQ1 eps fun_def
    _funCtx . Ctx.ix fun_name .= fun_def
    pure NamedFunDef{..}

annotateProgWithErrorBudgetWith ::
  ( m ~ Either String
  , ext' ~ AnnFailProb ext
  , m' ~ AnnotateMonad ext ext'
  , size ~ SizeType ext
  , prec ~ PrecType ext
  , ErrorReqs size prec
  ) =>
  (FailProb prec -> NamedFunDef ext' -> m' a) ->
  FailProb prec ->
  Program ext ->
  m (Program (AnnFailProb ext))
annotateProgWithErrorBudgetWith annotater eps prog = do
  -- start with program with each primitive annotated with fail prob = 1
  prog' <- annotateProgWith (_exts (annFixedEps (failProb 1.0))) prog

  -- pass-through the budget, updating each eps (with the minimum) as required
  let env = default_ -- not used
  let st =
        default_
          & (_funCtx .~ programToFunCtx prog') -- the initial 1 annotated program.

  -- run the budget splitting on the main function
  let Program fs = prog'
      main_fn = last fs
  (_, st', ()) <- runRWST (annotater eps main_fn) env st

  -- extract program from the final state
  pure $ st' ^. _funCtx . to funCtxToNamedFuns . to Program

annotateProgWithErrorBudgetU ::
  ( m ~ Either String
  , AnnotateWithErrorBudgetU ext
  , size ~ SizeType ext
  , prec ~ PrecType ext
  , ErrorReqs size prec
  ) =>
  FailProb prec ->
  Program ext ->
  m (Program (AnnFailProb ext))
annotateProgWithErrorBudgetU = annotateProgWithErrorBudgetWith annEpsU1

annotateProgWithErrorBudget ::
  ( m ~ Either String
  , AnnotateWithErrorBudgetU ext
  , AnnotateWithErrorBudgetQ ext
  , size ~ SizeType ext
  , prec ~ PrecType ext
  , ErrorReqs size prec
  ) =>
  FailProb prec ->
  Program ext ->
  m (Program (AnnFailProb ext))
annotateProgWithErrorBudget = annotateProgWithErrorBudgetWith annEpsQ1

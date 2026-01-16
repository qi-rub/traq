{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Traq.Analysis.Cost (
  module Traq.Analysis.Cost.Prelude,
  module Traq.Analysis.Cost.Unitary,
  module Traq.Analysis.Cost.Quantum,

  -- * Old cost functions

  -- * Types and Monad
  CostEnv,
  _costEnv,
  CostCalculator,

  -- * Precision Splitting Strategies
  PrecisionSplittingStrategy (..),
  HasPrecisionSplittingStrategy (..),
  HasNeedsEps,
  splitEps,
) where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ReaderT)
import GHC.Generics hiding (to)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Cost.Prelude
import Traq.Analysis.Cost.Quantum
import Traq.Analysis.Cost.Unitary
import Traq.Analysis.Error
import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Strategy for splitting the precison (eps/delta)
-- ================================================================================

-- | Predicate for checking if an expr/stmt/prog can fail
class HasNeedsEps p where
  type ExtensionType p

  needsEps ::
    ( sizeT ~ SizeType p
    , ext ~ ExtensionType p
    , MonadReader env m
    , HasFunCtx env ext
    ) =>
    p ->
    m Bool

instance HasNeedsEps (Expr ext) where
  type ExtensionType (Expr ext) = ext

  needsEps FunCallE{fname} =
    view (_funCtx . Ctx.at fname . singular _Just) >>= needsEps
  needsEps PrimCallE{} = return True
  needsEps _ = return False

instance HasNeedsEps (Stmt ext) where
  type ExtensionType (Stmt ext) = ext

  needsEps ExprS{expr} = needsEps expr
  needsEps (SeqS ss) = or <$> mapM needsEps ss
  needsEps IfThenElseS{s_true, s_false} = (||) <$> needsEps s_true <*> needsEps s_false

instance HasNeedsEps (FunDef ext) where
  type ExtensionType (FunDef ext) = ext

  needsEps FunDef{mbody} = case mbody of
    Nothing -> return False
    Just FunBody{body_stmt} -> needsEps body_stmt

splitEps ::
  forall primT sizeT precT errT env m.
  ( Floating precT
  , DivideError errT precT
  , MonadReader env m
  , HasFunCtx env primT
  , HasPrecisionSplittingStrategy env
  , HasNeedsEps (Stmt primT)
  , sizeT ~ SizeType env
  ) =>
  errT ->
  [Stmt primT] ->
  m [errT]
splitEps _ [] = return []
splitEps eps [_] = return [eps]
splitEps eps ss = do
  view _precSplitStrat >>= \case
    SplitSimple -> split_simple
    SplitUsingNeedsEps -> split_using_need_eps
 where
  split_simple = do
    let epss = eps & iterate (`divideError` 2) & tail & take (length ss - 1)
    return $ epss ++ [last epss]

  split_using_need_eps = do
    flags <- forM ss $ \s -> needsEps s
    let n_fail = length $ filter id flags

    let eps_each = if n_fail == 0 then 0 else eps `divideError` fromIntegral n_fail
    return $ map (\flag -> if flag then eps_each else 0) flags

-- ================================================================================
-- Env & Monad for Cost Analysis
-- ================================================================================

-- | Environment for cost analysis
data CostEnv ext = CostEnv (EvaluationEnv ext) PrecisionSplittingStrategy
  deriving (Generic, HasDefault)

-- Types and instances
type instance SizeType (CostEnv ext) = SizeType ext
type instance PrecType (CostEnv ext) = PrecType ext

instance HasEvaluationEnv (CostEnv ext) ext where
  _evaluationEnv focus (CostEnv e strat) = focus e <&> \e' -> CostEnv e' strat

instance HasFunCtx (CostEnv ext) ext where _funCtx = _evaluationEnv . _funCtx
instance HasFunInterpCtx (CostEnv ext) where _funInterpCtx = _evaluationEnv . _funInterpCtx

instance HasPrecisionSplittingStrategy (CostEnv ext) where
  _precSplitStrat focus (CostEnv f s) = focus s <&> \s' -> CostEnv f s'

-- Subtyping Lens
class HasCostEnv p ext | p -> ext where
  _costEnv :: Lens' p (CostEnv ext)

instance HasCostEnv (CostEnv ext) ext where _costEnv = id

-- | Monad for cost analysis
type CostCalculator ext = ReaderT (CostEnv ext) Maybe

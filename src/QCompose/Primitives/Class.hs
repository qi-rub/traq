module QCompose.Primitives.Class (
  PrimitiveImpl (..),

  -- * Type checker
) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import QCompose.Prelude
import QCompose.ProtoLang.Monad
import QCompose.ProtoLang.Syntax

-- | Environment for type checking
type TypeCheckEnv sizeT = (FunCtx sizeT, TypingCtx sizeT)

-- | Type check a primitive call
type PrimTypeChecker sizeT =
  forall m.
  (TypeCheckable sizeT, MonadReader (TypeCheckEnv sizeT) m) =>
  -- args
  [Ident] ->
  -- types of returned values
  m [VarType sizeT]

-- | Environment for evaluation.
type EvalEnv sizeT = (FunCtx sizeT, OracleInterp, ProgramState)

type PrimEvaluator sizeT =
  forall m.
  (MonadReader (EvalEnv sizeT) m) =>
  -- | arguments
  [Ident] ->
  m [Value]

type CostEnv sizeT = (FunCtx sizeT, OracleInterp)

type PrimCostCalculator sizeT costT =
  forall m.
  (MonadReader (CostEnv sizeT) m) =>
  -- | `eps`
  costT ->
  -- | cost
  m costT

{- | Implementation of a primitive.
 Provides the abstract cost formulas, typechecker, evaluator, and lowerings.
-}
data PrimitiveImpl sizeT costT = PrimitiveImpl
  { typeCheck :: PrimTypeChecker sizeT
  , evaluate :: PrimEvaluator sizeT
  , unitaryCost :: PrimCostCalculator sizeT costT
  , quantumCost :: PrimCostCalculator sizeT costT
  }

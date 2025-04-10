module QCompose.ProtoLang.Monad (
  -- * Type Checking
  TypingCtx,
  TypeChecker,
  TypeCheckable (..),

  -- * Evaluation
  ProgramState,
  OracleInterp,
  Evaluator,
  Executor,

  -- * Cost
) where

import QCompose.Control.MonadHelpers
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree
import QCompose.Prelude

import QCompose.ProtoLang.Syntax

-- | A context mapping variables to their types.
type TypingCtx a = Ctx.Context (VarType a)

-- | The TypeChecker monad
type TypeChecker a = MyStateT (TypingCtx a) (Either String)

class (Eq a, Show a, Num a) => TypeCheckable a where
  tbool :: VarType a
  tmax :: VarType a -> VarType a -> VarType a

instance TypeCheckable Integer where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

instance TypeCheckable Int where
  tbool = Fin 2
  tmax (Fin n) (Fin m) = Fin (max n m)

-- | The deterministic state of the program
type ProgramState = Ctx.Context Value

-- | Type for a function that implements the oracle
type OracleInterp = [Value] -> [Value]

-- | Environment for evaluation
type EvaluationEnv sizeT = (FunCtx sizeT, OracleInterp, ProgramState)

-- | Non-deterministic Evaluation Monad
type Evaluator = MyReaderT (EvaluationEnv SizeT) Tree.Tree

-- | Environment for evaluation
type ExecutionEnv sizeT = (FunCtx sizeT, OracleInterp)

type ExecutionState sizeT = ProgramState

-- | Non-deterministic Execution Monad
type Executor = MyReaderStateT (ExecutionEnv SizeT) (ExecutionState SizeT) Tree.Tree

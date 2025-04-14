module QCompose.ProtoLang.Monad (
  -- * Type Checking
  TypingCtx,
  TypeChecker,
  TypeCheckable (..),

  -- * Evaluation
  FunInterp,
  FunInterpCtx,
  ProgramState,
  ExecutionEnv,
  ExecutionState,
  Evaluator,
  Executor,

  -- * Cost
) where

import QCompose.Control.Monad
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

-- | Inject runtime data into a program
type FunInterp = [Value] -> [Value]

-- | A mapping of data injections
type FunInterpCtx = Ctx.Context FunInterp

-- | Environment for evaluation
type ExecutionEnv sizeT = (FunCtx sizeT, FunInterpCtx)

type ExecutionState sizeT = ProgramState

-- | Non-deterministic Execution Monad (i.e. no state)
type Evaluator = MyReaderT (ExecutionEnv SizeT) Tree.Tree

-- | Non-deterministic Execution Monad
type Executor = MyReaderStateT (ExecutionEnv SizeT) (ExecutionState SizeT) Tree.Tree

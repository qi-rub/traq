{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.QuantumCost (
  -- ** Classical-Quantum Compilation and Cost
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),
) where

import qualified Numeric.Algebra as Alg

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.TypeCheck
import Traq.Primitives.Class.UnitaryCost
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Quantum Compiler: Costs, Error.
-- --------------------------------------------------------------------------------

{- | Worst-case Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , A.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumHavocCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function's quantum compilation.
  quantumQueryCostsQuantum :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Bound on number of queries made to each function's unitary compilation.
  quantumQueryCostsUnitary :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  quantumExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  quantumExprCosts _ _ = Alg.zero

{- | Expected Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , A.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumExpCostPrim prim size prec
  where
  {- Bound on the expected number of queries made to each function's quantum compilation.
  This is a random variable over the inputs to each function.
  -}
  quantumExpQueryCostsQuantum ::
    forall shape m.
    ( shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape [([P.Value size], prec)]

  -- | Bound on the expected number of queries made to each function's unitary compilation.
  quantumExpQueryCostsUnitary ::
    forall shape m.
    (shape ~ PrimFnShape prim, m ~ P.EvaluationMonad prec) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  quantumExpExprCosts ::
    forall shape cost m.
    ( C.CostModel cost
    , prec ~ PrecType cost
    , shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    cost
  quantumExpExprCosts _ _ _ = Alg.zero

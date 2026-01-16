{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.QuantumCost (
  -- ** Classical-Quantum Compilation and Cost
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),
) where

import GHC.Generics

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.TypeCheck
import Traq.Primitives.Class.UnitaryCost
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Quantum Worst Case Costs
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
  default quantumQueryCostsQuantum ::
    ( Generic prim
    , GQuantumHavocCostPrim (Rep prim) size prec
    ) =>
    prim ->
    A.FailProb prec ->
    PrimFnShape prim prec
  quantumQueryCostsQuantum prim = reshapeUnsafe . gquantumQueryCostsQuantum (from prim)

  -- | Bound on number of queries made to each function's unitary compilation.
  quantumQueryCostsUnitary :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)
  default quantumQueryCostsUnitary ::
    ( Generic prim
    , GQuantumHavocCostPrim (Rep prim) size prec
    ) =>
    prim ->
    A.FailProb prec ->
    PrimFnShape prim (UnitaryQueries prec)
  quantumQueryCostsUnitary prim = reshapeUnsafe . gquantumQueryCostsUnitary (from prim)

  -- | Cost of all additional operations.
  quantumExprCosts :: (C.CostModel cost, prec ~ PrecType cost) => prim -> A.FailProb prec -> cost
  default quantumExprCosts ::
    ( Generic prim
    , GQuantumHavocCostPrim (Rep prim) size prec
    , C.CostModel cost
    , prec ~ PrecType cost
    ) =>
    prim ->
    A.FailProb prec ->
    cost
  quantumExprCosts prim = gquantumExprCosts (from prim)

class GQuantumHavocCostPrim f size prec | f -> size prec where
  gquantumQueryCostsQuantum :: f prim -> A.FailProb prec -> [prec]
  gquantumQueryCostsUnitary :: f prim -> A.FailProb prec -> [UnitaryQueries prec]
  gquantumExprCosts :: (C.CostModel cost, prec ~ PrecType cost) => f prim -> A.FailProb prec -> cost

instance (GQuantumHavocCostPrim a size prec, GQuantumHavocCostPrim b size prec) => GQuantumHavocCostPrim (a :+: b) size prec where
  gquantumQueryCostsQuantum (L1 x) = gquantumQueryCostsQuantum x
  gquantumQueryCostsQuantum (R1 x) = gquantumQueryCostsQuantum x

  gquantumQueryCostsUnitary (L1 x) = gquantumQueryCostsUnitary x
  gquantumQueryCostsUnitary (R1 x) = gquantumQueryCostsUnitary x

  gquantumExprCosts (L1 x) = gquantumExprCosts x
  gquantumExprCosts (R1 x) = gquantumExprCosts x

instance (GQuantumHavocCostPrim f size prec) => GQuantumHavocCostPrim (M1 i c f) size prec where
  gquantumQueryCostsQuantum (M1 x) = gquantumQueryCostsQuantum x

  gquantumQueryCostsUnitary (M1 x) = gquantumQueryCostsUnitary x

  gquantumExprCosts (M1 x) = gquantumExprCosts x

instance (QuantumHavocCostPrim a size prec) => GQuantumHavocCostPrim (K1 i a) size prec where
  gquantumQueryCostsQuantum (K1 x) = shapeToList . quantumQueryCostsQuantum x

  gquantumQueryCostsUnitary (K1 x) = shapeToList . quantumQueryCostsUnitary x

  gquantumExprCosts (K1 x) = quantumExprCosts x

-- --------------------------------------------------------------------------------
-- Quantum Expected Costs
-- --------------------------------------------------------------------------------

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
  default quantumExpQueryCostsQuantum ::
    forall shape m.
    (Generic prim, GQuantumExpCostPrim (Rep prim) size prec) =>
    ( shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape [([P.Value size], prec)]
  quantumExpQueryCostsQuantum prim eps = reshapeUnsafe . gquantumExpQueryCostsQuantum (from prim) eps . shapeToList

  -- | Bound on the expected number of queries made to each function's unitary compilation.
  quantumExpQueryCostsUnitary ::
    forall shape m.
    (shape ~ PrimFnShape prim, m ~ P.EvaluationMonad prec) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape (UnitaryQueries prec)
  default quantumExpQueryCostsUnitary ::
    forall shape m.
    (Generic prim, GQuantumExpCostPrim (Rep prim) size prec) =>
    (shape ~ PrimFnShape prim, m ~ P.EvaluationMonad prec) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape (UnitaryQueries prec)
  quantumExpQueryCostsUnitary prim eps = reshapeUnsafe . gquantumExpQueryCostsUnitary (from prim) eps . shapeToList

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
  default quantumExpExprCosts ::
    forall shape cost m.
    (Generic prim, GQuantumExpCostPrim (Rep prim) size prec) =>
    ( C.CostModel cost
    , prec ~ PrecType cost
    , shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    cost
  quantumExpExprCosts prim eps = gquantumExpExprCosts (from prim) eps . shapeToList

class GQuantumExpCostPrim f size prec | f -> size prec where
  gquantumExpQueryCostsQuantum ::
    (m ~ P.EvaluationMonad prec) =>
    f prim ->
    A.FailProb prec ->
    [[P.Value size] -> m [P.Value size]] ->
    [[([P.Value size], prec)]]
  gquantumExpQueryCostsUnitary ::
    (m ~ P.EvaluationMonad prec) =>
    f prim ->
    A.FailProb prec ->
    [[P.Value size] -> m [P.Value size]] ->
    [UnitaryQueries prec]
  gquantumExpExprCosts ::
    (m ~ P.EvaluationMonad prec) =>
    (C.CostModel cost, prec ~ PrecType cost) =>
    f prim ->
    A.FailProb prec ->
    [[P.Value size] -> m [P.Value size]] ->
    cost

instance (GQuantumExpCostPrim a size prec, GQuantumExpCostPrim b size prec) => GQuantumExpCostPrim (a :+: b) size prec where
  gquantumExpQueryCostsQuantum (L1 x) = gquantumExpQueryCostsQuantum x
  gquantumExpQueryCostsQuantum (R1 x) = gquantumExpQueryCostsQuantum x

  gquantumExpQueryCostsUnitary (L1 x) = gquantumExpQueryCostsUnitary x
  gquantumExpQueryCostsUnitary (R1 x) = gquantumExpQueryCostsUnitary x

  gquantumExpExprCosts (L1 x) = gquantumExpExprCosts x
  gquantumExpExprCosts (R1 x) = gquantumExpExprCosts x

instance (GQuantumExpCostPrim f size prec) => GQuantumExpCostPrim (M1 i c f) size prec where
  gquantumExpQueryCostsQuantum (M1 x) = gquantumExpQueryCostsQuantum x

  gquantumExpQueryCostsUnitary (M1 x) = gquantumExpQueryCostsUnitary x

  gquantumExpExprCosts (M1 x) = gquantumExpExprCosts x

instance (QuantumExpCostPrim a size prec) => GQuantumExpCostPrim (K1 i a) size prec where
  gquantumExpQueryCostsQuantum (K1 x) eps = shapeToList . quantumExpQueryCostsQuantum x eps . reshapeUnsafe

  gquantumExpQueryCostsUnitary (K1 x) eps = shapeToList . quantumExpQueryCostsUnitary x eps . reshapeUnsafe

  gquantumExpExprCosts (K1 x) eps = quantumExpExprCosts x eps . reshapeUnsafe

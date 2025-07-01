{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.ProtoLang.Cost (
  -- * Unitary Cost
  unitaryQueryCost,
  unitaryQueryCostE,
  unitaryQueryCostS,

  -- * Quantum Cost

  -- ** Expected cost
  quantumQueryCost,
  quantumQueryCostE,
  quantumQueryCostS,

  -- ** Quantum Worst case Cost
  quantumMaxQueryCost,
  quantumMaxQueryCostE,
  quantumMaxQueryCostS,

  -- ** Bound on runtime
  quantumQueryCostBound,

  -- * Types and Monad
  OracleName,
  OracleTicks,

  -- ** Static
  StaticCostEnv,
  UnitaryCostCalculator,
  UnitaryCostablePrimitive (..),
  QuantumMaxCostCalculator,
  QuantumMaxCostablePrimitive (..),

  -- ** Dynamic
  DynamicCostEnv,
  extractUEnv,
  QuantumCostCalculator,
  QuantumCostablePrimitive (..),
) where

import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.TypeCheck

detExtract :: (Foldable t) => t a -> a
detExtract xs = case toList xs of
  [x] -> x
  _ -> error "unexpected non-determinism"

{-# DEPRECATED unsafeLookupFun "Remove" #-}
unsafeLookupFun :: Ident -> FunCtx primsT sizeT -> FunDef primsT sizeT
unsafeLookupFun fname funCtx = funCtx ^. Ctx.at fname . singular _Just

-- ================================================================================
-- Unitary Cost
-- ================================================================================

-- | Name of the oracle to compute the number of queries to.
type OracleName = Ident

-- | Tick constant for each oracle.
type OracleTicks costT = Map.Map Ident costT

-- | Environment to compute the unitary cost
type StaticCostEnv primsT sizeT costT = (FunCtx primsT sizeT, OracleTicks costT)

_funCtx :: Lens' (StaticCostEnv primsT sizeT costT) (FunCtx primsT sizeT)
_funCtx = _1

_oracleTicks :: Lens' (StaticCostEnv primsT sizeT costT) (OracleTicks costT)
_oracleTicks = _2

-- | Monad to compute unitary cost.
type UnitaryCostCalculator primsT sizeT costT = MyReaderT (StaticCostEnv primsT sizeT costT) Maybe

-- | Primitives that have a unitary cost
class
  (TypeCheckablePrimitive primT sizeT, Show costT) =>
  UnitaryCostablePrimitive primsT primT sizeT costT
  where
  unitaryQueryCostPrimitive ::
    costT ->
    primT ->
    [Ident] ->
    UnitaryCostCalculator primsT sizeT costT costT

instance (Show costT) => UnitaryCostablePrimitive primsT Void sizeT costT where
  unitaryQueryCostPrimitive _ = absurd

-- | Evaluate the query cost of an expression
unitaryQueryCostE ::
  forall primsT sizeT costT m.
  ( Num sizeT
  , Floating costT
  , UnitaryCostablePrimitive primsT primsT sizeT costT
  , m ~ UnitaryCostCalculator primsT sizeT costT
  ) =>
  -- | precision
  costT ->
  -- | expression @E@
  Expr primsT sizeT ->
  m costT
unitaryQueryCostE delta FunCallE{fun_kind = FunctionCall fname} = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . singular _Just
  case mbody of
    Nothing -> view $ _oracleTicks . at fname . to (fromMaybe 0) -- declaration: use tick value (or 0 if not specified)
    Just FunBody{body_stmt} -> (2 *) <$> unitaryQueryCostS (delta / 2) body_stmt -- def: compute using body
unitaryQueryCostE delta FunCallE{fun_kind = PrimitiveCall prim, args} =
  unitaryQueryCostPrimitive delta prim args
-- zero-cost expressions
unitaryQueryCostE _ VarE{} = return 0
unitaryQueryCostE _ ConstE{} = return 0
unitaryQueryCostE _ UnOpE{} = return 0
unitaryQueryCostE _ BinOpE{} = return 0
unitaryQueryCostE _ TernaryE{} = return 0

-- Evaluate the query cost of a statement
unitaryQueryCostS ::
  forall primsT sizeT costT m.
  ( Num sizeT
  , Floating costT
  , UnitaryCostablePrimitive primsT primsT sizeT costT
  , m ~ UnitaryCostCalculator primsT sizeT costT
  ) =>
  -- | precision (l2-norm)
  costT ->
  -- | statement @S@
  Stmt primsT sizeT ->
  m costT
unitaryQueryCostS delta ExprS{expr} = unitaryQueryCostE delta expr
unitaryQueryCostS delta IfThenElseS{s_true, s_false} = do
  cost_true <- unitaryQueryCostS delta s_true
  cost_false <- unitaryQueryCostS delta s_false
  return $ cost_true + cost_false
unitaryQueryCostS _ (SeqS []) = return 0
unitaryQueryCostS delta (SeqS [s]) = unitaryQueryCostS delta s
unitaryQueryCostS delta (SeqS (s : ss)) = do
  cost_s <- unitaryQueryCostS (delta / 2) s
  cost_rest <- unitaryQueryCostS (delta / 2) (SeqS ss)
  return $ cost_s + cost_rest

unitaryQueryCost ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Floating costT
  , UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  -- | precision (l2-norm)
  costT ->
  -- | program @P@
  Program primsT sizeT ->
  -- | oracle ticks
  OracleTicks costT ->
  costT
unitaryQueryCost delta Program{funCtx, stmt} ticks =
  let env = (funCtx, ticks)
   in unitaryQueryCostS delta stmt `runMyReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Max Cost
-- ================================================================================

-- Environment to compute the max quantum cost (input independent)
type QuantumMaxCostCalculator primsT sizeT costT = MyReaderT (StaticCostEnv primsT sizeT costT) Maybe

-- | Primitives that have a quantum max cost
class
  (UnitaryCostablePrimitive primsT primT sizeT costT) =>
  QuantumMaxCostablePrimitive primsT primT sizeT costT
  where
  quantumMaxQueryCostPrimitive ::
    costT ->
    primT ->
    QuantumMaxCostCalculator primsT sizeT costT costT

instance (Show costT) => QuantumMaxCostablePrimitive primsT Void sizeT costT where
  quantumMaxQueryCostPrimitive _ = absurd

quantumMaxQueryCostE ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Ord costT
  , Floating costT
  , QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | statement @S@
  Expr primsT sizeT ->
  QuantumMaxCostCalculator primsT sizeT costT costT
quantumMaxQueryCostE eps FunCallE{fun_kind = FunctionCall fname} = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . singular _Just
  case mbody of
    Nothing -> view $ _oracleTicks . at fname . to (fromMaybe 0) -- declaration: use tick value (or 0 if not specified)
    Just FunBody{body_stmt} -> quantumMaxQueryCostS eps body_stmt -- def: compute using body

-- -- known cost formulas
quantumMaxQueryCostE eps FunCallE{fun_kind = PrimitiveCall prim} =
  quantumMaxQueryCostPrimitive eps prim
-- -- zero-cost expressions
quantumMaxQueryCostE _ VarE{} = return 0
quantumMaxQueryCostE _ ConstE{} = return 0
quantumMaxQueryCostE _ UnOpE{} = return 0
quantumMaxQueryCostE _ BinOpE{} = return 0
quantumMaxQueryCostE _ TernaryE{} = return 0

quantumMaxQueryCostS ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Ord costT
  , Floating costT
  , QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | statement @S@
  Stmt primsT sizeT ->
  QuantumMaxCostCalculator primsT sizeT costT costT
quantumMaxQueryCostS eps ExprS{expr} = quantumMaxQueryCostE eps expr
quantumMaxQueryCostS eps IfThenElseS{s_true, s_false} =
  max <$> quantumMaxQueryCostS eps s_true <*> quantumMaxQueryCostS eps s_false
quantumMaxQueryCostS _ (SeqS []) = return 0
quantumMaxQueryCostS eps (SeqS [s]) = quantumMaxQueryCostS eps s
quantumMaxQueryCostS eps (SeqS (s : ss)) = do
  cost_s <- quantumMaxQueryCostS (eps / 2) s
  cost_ss <- quantumMaxQueryCostS (eps / 2) (SeqS ss)
  return $ cost_s + cost_ss

quantumMaxQueryCost ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Ord costT
  , Floating costT
  , QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  -- | failure probability `eps`
  costT ->
  -- | program `P`
  Program primsT sizeT ->
  -- | oracle ticks
  OracleTicks costT ->
  costT
quantumMaxQueryCost a_eps Program{funCtx, stmt} ticks =
  let env = (funCtx, ticks)
   in quantumMaxQueryCostS a_eps stmt `runMyReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Cost
-- ================================================================================

-- Environment to compute the quantum cost (input dependent)
type DynamicCostEnv primsT sizeT costT = (StaticCostEnv primsT sizeT costT, FunInterpCtx, OracleTicks costT)

extractUEnv :: Lens' (DynamicCostEnv primsT sizeT costT) (StaticCostEnv primsT sizeT costT)
extractUEnv = _1

type QuantumCostCalculator primsT sizeT costT = MyReaderT (DynamicCostEnv primsT sizeT costT) Maybe

-- | Primitives that have a input dependent expected quantum cost
class
  ( QuantumMaxCostablePrimitive primsT primT sizeT costT
  , EvaluatablePrimitive primsT primT
  ) =>
  QuantumCostablePrimitive primsT primT sizeT costT
  where
  quantumQueryCostPrimitive ::
    costT ->
    primT ->
    [Value] ->
    QuantumCostCalculator primsT sizeT costT costT

instance (Show costT) => QuantumCostablePrimitive primsT Void sizeT costT where
  quantumQueryCostPrimitive _ = absurd

quantumQueryCostE ::
  forall primsT costT m.
  ( Floating costT
  , QuantumCostablePrimitive primsT primsT SizeT costT
  , m ~ QuantumCostCalculator primsT SizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | state \( \sigma \)
  ProgramState ->
  -- | statement @S@
  Expr primsT SizeT ->
  m costT
quantumQueryCostE eps sigma FunCallE{fun_kind = FunctionCall f, args} = do
  mtick <- view $ _3 . at f
  case mtick of
    Just tick -> return tick
    Nothing ->
      view (extractUEnv . _funCtx . to (unsafeLookupFun f) . to mbody) >>= \case
        Nothing -> return 0 -- no cost for injected data
        Just FunBody{param_names, body_stmt} -> do
          let vs = args & map (\x -> sigma ^. Ctx.at x . singular _Just)
          -- bind the arguments to the parameter names
          let omega = Ctx.fromList $ zip param_names vs
          quantumQueryCostS eps omega body_stmt

-- -- known cost formulas
quantumQueryCostE eps sigma FunCallE{fun_kind = PrimitiveCall prim, args} = do
  let vals = [sigma ^. Ctx.at x . singular _Just | x <- args]
  quantumQueryCostPrimitive eps prim vals
-- -- zero-cost expressions
quantumQueryCostE _ _ VarE{} = return 0
quantumQueryCostE _ _ ConstE{} = return 0
quantumQueryCostE _ _ UnOpE{} = return 0
quantumQueryCostE _ _ BinOpE{} = return 0
quantumQueryCostE _ _ TernaryE{} = return 0

quantumQueryCostS ::
  forall primsT costT m.
  ( Floating costT
  , QuantumCostablePrimitive primsT primsT SizeT costT
  , m ~ QuantumCostCalculator primsT SizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | state \( \sigma \)
  ProgramState ->
  -- | statement @S@
  Stmt primsT SizeT ->
  QuantumCostCalculator primsT SizeT costT costT
quantumQueryCostS eps sigma ExprS{expr} = quantumQueryCostE eps sigma expr
quantumQueryCostS eps sigma IfThenElseS{cond, s_true, s_false} =
  let s = if sigma ^. Ctx.at cond /= Just 0 then s_true else s_false
   in quantumQueryCostS eps sigma s
quantumQueryCostS _ _ (SeqS []) = return 0
quantumQueryCostS eps sigma (SeqS [s]) = quantumQueryCostS eps sigma s
quantumQueryCostS eps sigma (SeqS (s : ss)) = do
  cost_s <- quantumQueryCostS (eps / 2) sigma s

  funCtx <- view $ extractUEnv . _funCtx
  interpCtx <- view _2
  let sigma' = detExtract $ runProgram Program{funCtx, stmt = s} interpCtx sigma

  cost_ss <- quantumQueryCostS (eps / 2) sigma' (SeqS ss)
  return $ cost_s + cost_ss

quantumQueryCost ::
  forall primsT costT.
  (Floating costT, QuantumCostablePrimitive primsT primsT SizeT costT) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primsT SizeT ->
  -- | oracle ticks
  OracleTicks costT ->
  -- | data injections
  FunInterpCtx ->
  -- | state \( \sigma \)
  ProgramState ->
  costT
quantumQueryCost a_eps Program{funCtx, stmt} ticks interpCtx sigma =
  let env = ((funCtx, ticks), interpCtx, ticks)
   in quantumQueryCostS a_eps sigma stmt `runMyReaderT` env ^. singular _Just

-- | The bound on the true expected runtime which fails with probability <= \eps.
quantumQueryCostBound ::
  forall primsT costT.
  ( Ord costT
  , Floating costT
  , QuantumCostablePrimitive primsT primsT SizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primsT SizeT ->
  -- | oracle ticks
  OracleTicks costT ->
  -- | data injections
  FunInterpCtx ->
  -- | state \( \sigma \)
  ProgramState ->
  costT
quantumQueryCostBound a_eps p ticks interp_ctx sigma =
  (1 - a_eps) * quantumQueryCost a_eps p ticks interp_ctx sigma
    + a_eps * quantumMaxQueryCost a_eps p ticks

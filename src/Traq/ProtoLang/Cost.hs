{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.ProtoLang.Cost (
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
  UnitaryCostEnv,
  QuantumMaxCostEnv,
  QuantumCostEnv,

  -- ** Data Types
  OracleTicks,

  -- ** Lenses
  _classicalTicks,
  _unitaryTicks,
  _funInterpCtx,
  _unitaryCostEnv,
  _quantumMaxCostEnv,

  -- ** Cost Monads
  UnitaryCostCalculator,
  UnitaryCostablePrimitive (..),
  QuantumMaxCostCalculator,
  QuantumMaxCostablePrimitive (..),
  QuantumCostCalculator,
  QuantumCostablePrimitive (..),

  -- * Precision Splitting Strategies
  PrecisionSplittingStrategy (..),
  HasPrecisionSplittingStrategy (..),
  HasNeedsEps (..),
  splitEps,
) where

import Control.Monad (foldM, forM, zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Tree as Tree

import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck

-- ================================================================================
-- Lenses for accessing the "tick" costs and semantics function declarations
-- ================================================================================

-- | Tick constant for each oracle.
type OracleTicks costT = Map.Map Ident costT

-- | Lens to access the classical ticks from the context
class HasClassicalTicks p where
  _classicalTicks :: (costT ~ CostType p) => Lens' p (OracleTicks costT)

-- | Lens to access the unitary ticks from the context
class HasUnitaryTicks p where
  _unitaryTicks :: (costT ~ CostType p) => Lens' p (OracleTicks costT)

-- ================================================================================
-- Strategy for splitting the precison (eps/delta)
-- ================================================================================
data PrecisionSplittingStrategy = SplitSimple | SplitUsingNeedsEps
  deriving (Eq, Read, Show)

instance HasDefault PrecisionSplittingStrategy where
  default_ = SplitSimple

class HasPrecisionSplittingStrategy p where
  _precSplitStrat :: Lens' p PrecisionSplittingStrategy

-- | Predicate for checking if a expr/stmt/prog can fail
class HasNeedsEps p where
  needsEps ::
    ( primT ~ PrimitiveType p
    , sizeT ~ SizeType p
    , MonadReader env m
    , HasFunCtx env
    , sizeT ~ SizeType env
    , primT ~ PrimitiveType env
    ) =>
    p ->
    m Bool

instance HasNeedsEps Void where
  needsEps = absurd

instance HasNeedsEps (Expr primT sizeT) where
  needsEps FunCallE{fun_kind = FunctionCall f} =
    view (_funCtx . Ctx.at f . singular _Just) >>= needsEps
  needsEps FunCallE{fun_kind = PrimitiveCall _} = return True
  needsEps _ = return False

instance HasNeedsEps (Stmt primT sizeT) where
  needsEps ExprS{expr} = needsEps expr
  needsEps (SeqS ss) = or <$> mapM needsEps ss
  needsEps IfThenElseS{s_true, s_false} = (||) <$> needsEps s_true <*> needsEps s_false

instance HasNeedsEps (FunDef primT sizeT) where
  needsEps FunDef{mbody} = case mbody of
    Nothing -> return False
    Just FunBody{body_stmt} -> needsEps body_stmt

splitEps ::
  forall primT sizeT costT env m.
  ( Floating costT
  , Monad m
  , MonadReader env m
  , HasFunCtx env
  , HasPrecisionSplittingStrategy env
  , HasNeedsEps (Stmt primT sizeT)
  , primT ~ PrimitiveType env
  , sizeT ~ SizeType env
  ) =>
  costT ->
  [Stmt primT sizeT] ->
  m [costT]
splitEps _ [] = return []
splitEps eps [_] = return [eps]
splitEps eps ss = do
  view _precSplitStrat >>= \case
    SplitSimple -> split_simple
    SplitUsingNeedsEps -> split_using_need_eps
 where
  split_simple = do
    let epss = eps & iterate (/ 2) & tail & take (length ss - 1)
    return $ epss ++ [last epss]

  split_using_need_eps = do
    flags <- forM ss $ \s -> needsEps s
    let n_fail = length $ filter id flags

    let eps_each = if n_fail == 0 then 0 else eps / fromIntegral n_fail
    return $ map (\flag -> if flag then eps_each else 0) flags

-- ================================================================================
-- Unitary Cost
-- ================================================================================

-- | Environment to compute the unitary cost
data UnitaryCostEnv primT sizeT costT = UnitaryCostEnv (FunCtx primT sizeT) (OracleTicks costT) PrecisionSplittingStrategy

instance HasDefault (UnitaryCostEnv primT sizeT costT) where
  default_ = UnitaryCostEnv default_ default_ default_

-- Types and instances
type instance PrimitiveType (UnitaryCostEnv primT sizeT costT) = primT
type instance SizeType (UnitaryCostEnv primT sizeT costT) = sizeT
type instance CostType (UnitaryCostEnv primT sizeT costT) = costT

instance HasFunCtx (UnitaryCostEnv primT sizeT costT) where
  _funCtx focus (UnitaryCostEnv f u s) = focus f <&> \f' -> UnitaryCostEnv f' u s

instance HasUnitaryTicks (UnitaryCostEnv primT sizeT costT) where
  _unitaryTicks focus (UnitaryCostEnv f u s) = focus u <&> \u' -> UnitaryCostEnv f u' s

instance HasPrecisionSplittingStrategy (UnitaryCostEnv primT sizeT costT) where
  _precSplitStrat focus (UnitaryCostEnv f u s) = focus s <&> \s' -> UnitaryCostEnv f u s'

-- Subtyping Lens
class HasUnitaryCostEnv p where
  _unitaryCostEnv ::
    ( primT ~ PrimitiveType p
    , sizeT ~ SizeType p
    , costT ~ CostType p
    ) =>
    Lens' p (UnitaryCostEnv primT sizeT costT)

instance HasUnitaryCostEnv (UnitaryCostEnv p s c) where _unitaryCostEnv = id

-- | Monad to compute unitary cost.
type UnitaryCostCalculator primsT sizeT costT = ReaderT (UnitaryCostEnv primsT sizeT costT) Maybe

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
  cost_half <- case mbody of
    Nothing -> view $ _unitaryTicks . at fname . to (fromMaybe 0) -- declaration: use tick value (or 0 if not specified)
    Just FunBody{body_stmt} -> unitaryQueryCostS (delta / 2) body_stmt -- def: compute using body
  return $ 2 * cost_half
unitaryQueryCostE delta FunCallE{fun_kind = PrimitiveCall prim, args} =
  unitaryQueryCostPrimitive delta prim args
-- zero-cost expressions
unitaryQueryCostE _ BasicExprE{} = return 0

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
unitaryQueryCostS delta (SeqS ss) = do
  delta_each <- splitEps delta ss
  cs <- zipWithM unitaryQueryCostS delta_each ss
  return $ sum cs

unitaryQueryCost ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Floating costT
  , UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | precision (l2-norm)
  costT ->
  -- | program @P@
  Program primsT sizeT ->
  -- | oracle ticks
  OracleTicks costT ->
  costT
unitaryQueryCost strat delta Program{funCtx, stmt} ticks =
  let env =
        default_
          & (_funCtx .~ funCtx)
          & (_unitaryTicks .~ ticks)
          & (_precSplitStrat .~ strat)
   in unitaryQueryCostS delta stmt `runReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Max Cost
-- ================================================================================

-- | Environment to compute the worst case quantum cost
data QuantumMaxCostEnv primT sizeT costT
  = QuantumMaxCostEnv
      (UnitaryCostEnv primT sizeT costT) -- unitary enviroment
      (OracleTicks costT) -- classical ticks

instance HasDefault (QuantumMaxCostEnv primT sizeT costT) where
  default_ = QuantumMaxCostEnv default_ default_

-- instances
type instance PrimitiveType (QuantumMaxCostEnv primT sizeT costT) = primT
type instance SizeType (QuantumMaxCostEnv primT sizeT costT) = sizeT
type instance CostType (QuantumMaxCostEnv primT sizeT costT) = costT

instance HasUnitaryCostEnv (QuantumMaxCostEnv primT sizeT costT) where
  _unitaryCostEnv focus (QuantumMaxCostEnv u t) = focus u <&> \u' -> QuantumMaxCostEnv u' t

instance HasClassicalTicks (QuantumMaxCostEnv primT sizeT costT) where
  _classicalTicks focus (QuantumMaxCostEnv u t) = focus t <&> QuantumMaxCostEnv u

instance HasFunCtx (QuantumMaxCostEnv primT sizeT costT) where _funCtx = _unitaryCostEnv . _funCtx
instance HasUnitaryTicks (QuantumMaxCostEnv primT sizeT costT) where _unitaryTicks = _unitaryCostEnv . _unitaryTicks
instance HasPrecisionSplittingStrategy (QuantumMaxCostEnv primT sizeT costT) where _precSplitStrat = _unitaryCostEnv . _precSplitStrat

-- lens
class HasQuantumMaxCostEnv p where
  _quantumMaxCostEnv ::
    ( primT ~ PrimitiveType p
    , sizeT ~ SizeType p
    , costT ~ CostType p
    ) =>
    Lens' p (QuantumMaxCostEnv primT sizeT costT)

instance HasQuantumMaxCostEnv (QuantumMaxCostEnv primT sizeT costT) where _quantumMaxCostEnv = id

-- Environment to compute the max quantum cost (input independent)
type QuantumMaxCostCalculator primsT sizeT costT = ReaderT (QuantumMaxCostEnv primsT sizeT costT) Maybe

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
    Nothing -> view $ _classicalTicks . at fname . to (fromMaybe 0) -- declaration: use tick value (or 0 if not specified)
    Just FunBody{body_stmt} -> quantumMaxQueryCostS eps body_stmt -- def: compute using body

-- -- known cost formulas
quantumMaxQueryCostE eps FunCallE{fun_kind = PrimitiveCall prim} =
  quantumMaxQueryCostPrimitive eps prim
-- -- zero-cost expressions
quantumMaxQueryCostE _ BasicExprE{} = return 0

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
quantumMaxQueryCostS eps (SeqS ss) = do
  eps_each <- splitEps eps ss
  cs <- zipWithM quantumMaxQueryCostS eps_each ss
  return $ sum cs

quantumMaxQueryCost ::
  forall primsT sizeT costT.
  ( Num sizeT
  , Ord costT
  , Floating costT
  , QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | failure probability `eps`
  costT ->
  -- | program `P`
  Program primsT sizeT ->
  -- | unitary ticks
  OracleTicks costT ->
  -- | classical ticks
  OracleTicks costT ->
  costT
quantumMaxQueryCost strat a_eps Program{funCtx, stmt} uticks cticks =
  let env =
        default_
          & (_funCtx .~ funCtx)
          & (_unitaryTicks .~ uticks)
          & (_classicalTicks .~ cticks)
          & (_precSplitStrat .~ strat)
   in quantumMaxQueryCostS a_eps stmt `runReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Cost
-- ================================================================================

-- Environment to compute the quantum cost (input dependent)
data QuantumCostEnv primsT sizeT costT
  = QuantumCostEnv
      (QuantumMaxCostEnv primsT sizeT costT)
      (FunInterpCtx sizeT)

instance HasDefault (QuantumCostEnv primT sizeT costT) where
  default_ = QuantumCostEnv default_ default_

type instance PrimitiveType (QuantumCostEnv primsT sizeT costT) = primsT
type instance SizeType (QuantumCostEnv primsT sizeT costT) = sizeT
type instance CostType (QuantumCostEnv primsT sizeT costT) = costT

instance HasQuantumMaxCostEnv (QuantumCostEnv primsT sizeT costT) where
  _quantumMaxCostEnv focus (QuantumCostEnv e f) = focus e <&> \e' -> QuantumCostEnv e' f

instance HasFunInterpCtx (QuantumCostEnv primsT sizeT costT) where
  _funInterpCtx focus (QuantumCostEnv e f) = focus f <&> QuantumCostEnv e

instance HasUnitaryCostEnv (QuantumCostEnv primsT sizeT costT) where _unitaryCostEnv = _quantumMaxCostEnv . _unitaryCostEnv
instance HasFunCtx (QuantumCostEnv primsT sizeT costT) where _funCtx = _quantumMaxCostEnv . _funCtx
instance HasUnitaryTicks (QuantumCostEnv primsT sizeT costT) where _unitaryTicks = _quantumMaxCostEnv . _unitaryTicks
instance HasClassicalTicks (QuantumCostEnv primsT sizeT costT) where _classicalTicks = _quantumMaxCostEnv . _classicalTicks
instance HasPrecisionSplittingStrategy (QuantumCostEnv primsT sizeT costT) where _precSplitStrat = _quantumMaxCostEnv . _precSplitStrat

instance HasEvaluationEnv (QuantumCostEnv primsT sizeT costT) where
  _evaluationEnv = lens _get _set
   where
    _get ce =
      default_
        & (_funCtx .~ (ce ^. _funCtx))
        & (_funInterpCtx .~ (ce ^. _funInterpCtx))
    _set ce ee =
      ce
        & (_funCtx .~ (ee ^. _funCtx))
        & (_funInterpCtx .~ (ee ^. _funInterpCtx))

type QuantumCostCalculator primsT sizeT costT = ReaderT (QuantumCostEnv primsT sizeT costT) Maybe

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
    [Value sizeT] ->
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
  ProgramState SizeT ->
  -- | statement @S@
  Expr primsT SizeT ->
  m costT
quantumQueryCostE eps sigma FunCallE{fun_kind = FunctionCall f, args} =
  view (_funCtx . Ctx.at f . singular _Just . to mbody) >>= \case
    Nothing -> view $ _classicalTicks . at f . to (fromMaybe 0) -- tick value, default to 0
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
quantumQueryCostE _ _ BasicExprE{} = return 0

quantumQueryCostS ::
  forall primsT costT m.
  ( Floating costT
  , QuantumCostablePrimitive primsT primsT SizeT costT
  , m ~ QuantumCostCalculator primsT SizeT costT
  ) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | state \( \sigma \)
  ProgramState SizeT ->
  -- | statement @S@
  Stmt primsT SizeT ->
  QuantumCostCalculator primsT SizeT costT costT
quantumQueryCostS eps sigma ExprS{expr} = quantumQueryCostE eps sigma expr
quantumQueryCostS eps sigma IfThenElseS{cond, s_true, s_false} =
  let s = if valueToBool (sigma ^?! Ctx.at cond . singular _Just) then s_true else s_false
   in quantumQueryCostS eps sigma s
quantumQueryCostS eps sigma (SeqS ss) = do
  funCtx <- view _funCtx
  interpCtx <- view _funInterpCtx
  let stepS s sigma_s = Tree.detExtract $ runProgram Program{funCtx, stmt = s} interpCtx sigma_s

  eps_each <- splitEps eps ss

  (_, cs) <- (\r -> foldM r (sigma, 0) (zip ss eps_each)) $
    \(sigma_s, acc) (s, eps_s) -> do
      c <- quantumQueryCostS eps_s sigma_s s
      return (stepS s sigma_s, acc + c)
  return cs

quantumQueryCost ::
  forall primsT costT.
  (Floating costT, QuantumCostablePrimitive primsT primsT SizeT costT) =>
  PrecisionSplittingStrategy ->
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primsT SizeT ->
  -- | unitary ticks
  OracleTicks costT ->
  -- | classical ticks
  OracleTicks costT ->
  -- | data injections
  FunInterpCtx SizeT ->
  -- | state \( \sigma \)
  ProgramState SizeT ->
  costT
quantumQueryCost strat a_eps Program{funCtx, stmt} uticks cticks interpCtx sigma =
  let env =
        default_
          & (_funCtx .~ funCtx)
          & (_unitaryTicks .~ uticks)
          & (_classicalTicks .~ cticks)
          & (_precSplitStrat .~ strat)
          & (_funInterpCtx .~ interpCtx)
   in quantumQueryCostS a_eps sigma stmt `runReaderT` env ^. singular _Just

-- | The bound on the true expected runtime which fails with probability <= \eps.
quantumQueryCostBound ::
  forall primsT costT.
  ( Ord costT
  , Floating costT
  , QuantumCostablePrimitive primsT primsT SizeT costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primsT SizeT ->
  -- | unitary ticks
  OracleTicks costT ->
  -- | classical ticks
  OracleTicks costT ->
  -- | data injections
  FunInterpCtx SizeT ->
  -- | state \( \sigma \)
  ProgramState SizeT ->
  costT
quantumQueryCostBound strat a_eps p uticks cticks interp_ctx sigma =
  (1 - a_eps) * quantumQueryCost strat a_eps p uticks cticks interp_ctx sigma
    + a_eps * quantumMaxQueryCost strat a_eps p uticks cticks

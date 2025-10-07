{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

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

  -- * Types and Monad
  UnitaryCostEnv,
  QuantumMaxCostEnv,
  QuantumCostEnv,

  -- ** Lenses
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

  -- * Misc
  SizeToPrec (..),
) where

import Control.Monad (forM, zipWithM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (execStateT)
import Data.Void (Void, absurd)
import GHC.Generics hiding (to)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Probability as Prob
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CostModel.Class as C
import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck

class SizeToPrec sizeT precT where
  sizeToPrec :: sizeT -> precT

instance (Floating precT) => SizeToPrec Integer precT where sizeToPrec = fromIntegral
instance (Floating precT) => SizeToPrec Int precT where sizeToPrec = fromIntegral

instance (Show sizeT) => SizeToPrec (Sym.Sym sizeT) (Sym.Sym precT) where
  sizeToPrec s = Sym.var (show s)

-- ================================================================================
-- Strategy for splitting the precison (eps/delta)
-- ================================================================================
data PrecisionSplittingStrategy = SplitSimple | SplitUsingNeedsEps
  deriving (Eq, Read, Show)

instance HasDefault PrecisionSplittingStrategy where
  default_ = SplitSimple

-- Predicate for checking if a type has precision splitting strategy
class HasPrecisionSplittingStrategy p where
  _precSplitStrat :: Lens' p PrecisionSplittingStrategy

-- | Predicate for checking if an expr/stmt/prog can fail
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
  needsEps FunCallE{fname} =
    view (_funCtx . Ctx.at fname . singular _Just) >>= needsEps
  needsEps PrimCallE{} = return True
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
  forall primT sizeT precT env m.
  ( Floating precT
  , Monad m
  , MonadReader env m
  , HasFunCtx env
  , HasPrecisionSplittingStrategy env
  , HasNeedsEps (Stmt primT sizeT)
  , primT ~ PrimitiveType env
  , sizeT ~ SizeType env
  ) =>
  precT ->
  [Stmt primT sizeT] ->
  m [precT]
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
data UnitaryCostEnv primT sizeT precT = UnitaryCostEnv (FunCtx primT sizeT) PrecisionSplittingStrategy
  deriving (Generic, HasDefault)

-- Types and instances
type instance PrimitiveType (UnitaryCostEnv primT sizeT precT) = primT

type instance SizeType (UnitaryCostEnv primT sizeT precT) = sizeT

type instance PrecType (UnitaryCostEnv primT sizeT precT) = precT

instance HasFunCtx (UnitaryCostEnv primT sizeT precT) where
  _funCtx focus (UnitaryCostEnv f s) = focus f <&> \f' -> UnitaryCostEnv f' s

instance HasPrecisionSplittingStrategy (UnitaryCostEnv primT sizeT precT) where
  _precSplitStrat focus (UnitaryCostEnv f s) = focus s <&> \s' -> UnitaryCostEnv f s'

-- Subtyping Lens
class HasUnitaryCostEnv p where
  _unitaryCostEnv ::
    ( primT ~ PrimitiveType p
    , sizeT ~ SizeType p
    , precT ~ PrecType p
    ) =>
    Lens' p (UnitaryCostEnv primT sizeT precT)

instance HasUnitaryCostEnv (UnitaryCostEnv p s c) where _unitaryCostEnv = id

-- | Monad to compute unitary cost.
type UnitaryCostCalculator primsT sizeT precT = ReaderT (UnitaryCostEnv primsT sizeT precT) Maybe

-- --------------------------------------------------------------------------------
-- Unitary Cost: Primitives (with generics)
-- --------------------------------------------------------------------------------

-- | Primitives that have a unitary cost
class
  (TypeCheckablePrimitive primT, Show precT) =>
  UnitaryCostablePrimitive primT sizeT precT
  where
  unitaryQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ UnitaryCostCalculator primsT sizeT precT
    , UnitaryCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    ) =>
    precT ->
    primT ->
    m costT
  default unitaryQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ UnitaryCostCalculator primsT sizeT precT
    , UnitaryCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Generic primT
    , GUnitaryCostablePrimitive (Rep primT) sizeT precT
    ) =>
    precT ->
    primT ->
    m costT
  unitaryQueryCostPrimitive delta p = gunitaryQueryCostPrimitive delta (from p)

instance (Show precT) => UnitaryCostablePrimitive Void sizeT precT where
  unitaryQueryCostPrimitive _ = absurd

class GUnitaryCostablePrimitive f sizeT precT where
  gunitaryQueryCostPrimitive ::
    forall primsT primT costT m.
    ( m ~ UnitaryCostCalculator primsT sizeT precT
    , UnitaryCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    ) =>
    precT ->
    f primT ->
    m costT

instance
  (GUnitaryCostablePrimitive f1 sizeT precT, GUnitaryCostablePrimitive f2 sizeT precT) =>
  GUnitaryCostablePrimitive (f1 :+: f2) sizeT precT
  where
  gunitaryQueryCostPrimitive delta (L1 p) = gunitaryQueryCostPrimitive delta p
  gunitaryQueryCostPrimitive delta (R1 p) = gunitaryQueryCostPrimitive delta p

instance (GUnitaryCostablePrimitive f sizeT precT) => GUnitaryCostablePrimitive (M1 i c f) sizeT precT where
  gunitaryQueryCostPrimitive delta (M1 x) = gunitaryQueryCostPrimitive delta x

instance
  (UnitaryCostablePrimitive f sizeT precT) =>
  GUnitaryCostablePrimitive (K1 i f) sizeT precT
  where
  gunitaryQueryCostPrimitive delta (K1 x) = unitaryQueryCostPrimitive delta x

-- --------------------------------------------------------------------------------
-- Unitary Cost: Core Language
-- --------------------------------------------------------------------------------

-- | Evaluate the query cost of an expression
unitaryQueryCostE ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCostablePrimitive primsT sizeT precT
  , m ~ UnitaryCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision
  precT ->
  -- | expression @E@
  Expr primsT sizeT ->
  m costT
unitaryQueryCostE delta FunCallE{fname} = ((2.0 :: precT) Alg..*) <$> unitaryQueryCostF (delta / 2) fname
unitaryQueryCostE delta PrimCallE{prim} = unitaryQueryCostPrimitive delta prim
-- basic expressions
unitaryQueryCostE _ BasicExprE{basic_expr} = return $ C.callExpr C.Unitary basic_expr
unitaryQueryCostE _ RandomSampleE{distr_expr} = return $ C.callDistrExpr C.Unitary distr_expr
unitaryQueryCostE _ _ = error "TODO"

-- Evaluate the query cost of a statement
unitaryQueryCostS ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCostablePrimitive primsT sizeT precT
  , m ~ UnitaryCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision (l2-norm)
  precT ->
  -- | statement @S@
  Stmt primsT sizeT ->
  m costT
unitaryQueryCostS delta ExprS{expr} = unitaryQueryCostE delta expr
unitaryQueryCostS delta IfThenElseS{s_true, s_false} = do
  cost_true <- unitaryQueryCostS delta s_true
  cost_false <- unitaryQueryCostS delta s_false
  return $ cost_true Alg.+ cost_false
unitaryQueryCostS delta (SeqS ss) = do
  delta_each <- splitEps delta ss
  cs <- zipWithM unitaryQueryCostS delta_each ss
  return $ Alg.sum cs

-- Evaluate the query cost of a function
unitaryQueryCostF ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Floating precT
  , UnitaryCostablePrimitive primsT sizeT precT
  , m ~ UnitaryCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  -- | precision (l2-norm)
  precT ->
  -- | function name
  Ident ->
  m costT
-- declaration: use tick value (or 0 if not specified)
unitaryQueryCostF delta fname = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . non' (error "invalid function")
  case mbody of
    Nothing -> return $ C.query C.Unitary fname -- query an external function
    Just FunBody{body_stmt} -> unitaryQueryCostS delta body_stmt -- def: compute using body

unitaryQueryCost ::
  forall primsT sizeT precT costT.
  ( Num sizeT
  , Floating precT
  , UnitaryCostablePrimitive primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  ) =>
  PrecisionSplittingStrategy ->
  -- | precision (l2-norm)
  precT ->
  -- | program @P@
  Program primsT sizeT ->
  costT
unitaryQueryCost strat delta (Program fs) =
  let env =
        default_
          & (_funCtx .~ namedFunsToFunCtx fs)
          & (_precSplitStrat .~ strat)
   in unitaryQueryCostF delta (fun_name $ last fs)
        & (runReaderT ?? env)
        & (^?! _Just)

-- ================================================================================
-- Quantum Max Cost
-- ================================================================================

-- | Environment to compute the worst case quantum cost
newtype QuantumMaxCostEnv primT sizeT precT
  = QuantumMaxCostEnv
      (UnitaryCostEnv primT sizeT precT) -- unitary enviroment
  deriving (Generic, HasDefault)
{-# DEPRECATED QuantumMaxCostEnv "directly use UnitaryCostEnv (or rename to sth. like WorstCaseCostEnv" #-}

-- instances
type instance PrimitiveType (QuantumMaxCostEnv primT sizeT precT) = primT

type instance SizeType (QuantumMaxCostEnv primT sizeT precT) = sizeT

type instance PrecType (QuantumMaxCostEnv primT sizeT precT) = precT

instance HasUnitaryCostEnv (QuantumMaxCostEnv primT sizeT precT) where
  _unitaryCostEnv focus (QuantumMaxCostEnv u) = focus u <&> \u' -> QuantumMaxCostEnv u'

instance HasFunCtx (QuantumMaxCostEnv primT sizeT precT) where _funCtx = _unitaryCostEnv . _funCtx

instance HasPrecisionSplittingStrategy (QuantumMaxCostEnv primT sizeT precT) where _precSplitStrat = _unitaryCostEnv . _precSplitStrat

-- lens
class HasQuantumMaxCostEnv p where
  _quantumMaxCostEnv ::
    ( primT ~ PrimitiveType p
    , sizeT ~ SizeType p
    , precT ~ PrecType p
    ) =>
    Lens' p (QuantumMaxCostEnv primT sizeT precT)

instance HasQuantumMaxCostEnv (QuantumMaxCostEnv primT sizeT precT) where _quantumMaxCostEnv = id

-- Environment to compute the max quantum cost (input independent)
type QuantumMaxCostCalculator primsT sizeT precT = ReaderT (QuantumMaxCostEnv primsT sizeT precT) Maybe

-- --------------------------------------------------------------------------------
-- Quantum Max Cost: Primitives (with generics)
-- --------------------------------------------------------------------------------

-- | Primitives that have a quantum max cost
class
  (UnitaryCostablePrimitive primT sizeT precT) =>
  QuantumMaxCostablePrimitive primT sizeT precT
  where
  quantumMaxQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ QuantumMaxCostCalculator primsT sizeT precT
    , QuantumMaxCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    ) =>
    precT ->
    primT ->
    m costT
  default quantumMaxQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ QuantumMaxCostCalculator primsT sizeT precT
    , QuantumMaxCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    , Generic primT
    , GQuantumMaxCostablePrimitive (Rep primT) sizeT precT
    ) =>
    precT ->
    primT ->
    m costT
  quantumMaxQueryCostPrimitive eps p = gquantumMaxQueryCostPrimitive eps (from p)

instance (Show precT) => QuantumMaxCostablePrimitive Void sizeT precT where
  quantumMaxQueryCostPrimitive _ = absurd

class GQuantumMaxCostablePrimitive f sizeT precT where
  gquantumMaxQueryCostPrimitive ::
    forall primT primsT costT m.
    ( m ~ QuantumMaxCostCalculator primsT sizeT precT
    , QuantumMaxCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , SizeToPrec sizeT precT
    , Ord costT
    ) =>
    precT ->
    f primT ->
    m costT

instance
  (GQuantumMaxCostablePrimitive f1 sizeT precT, GQuantumMaxCostablePrimitive f2 sizeT precT) =>
  GQuantumMaxCostablePrimitive (f1 :+: f2) sizeT precT
  where
  gquantumMaxQueryCostPrimitive eps (L1 p) = gquantumMaxQueryCostPrimitive eps p
  gquantumMaxQueryCostPrimitive eps (R1 p) = gquantumMaxQueryCostPrimitive eps p

instance (GQuantumMaxCostablePrimitive f sizeT precT) => GQuantumMaxCostablePrimitive (M1 i c f) sizeT precT where
  gquantumMaxQueryCostPrimitive eps (M1 x) = gquantumMaxQueryCostPrimitive eps x

instance
  (QuantumMaxCostablePrimitive f sizeT precT) =>
  GQuantumMaxCostablePrimitive (K1 i f) sizeT precT
  where
  gquantumMaxQueryCostPrimitive eps (K1 x) = quantumMaxQueryCostPrimitive eps x

-- --------------------------------------------------------------------------------
-- Quantum Max Cost: Core Language
-- --------------------------------------------------------------------------------

quantumMaxQueryCostE ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumMaxCostablePrimitive primsT sizeT precT
  , m ~ QuantumMaxCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | statement @S@
  Expr primsT sizeT ->
  m costT
quantumMaxQueryCostE eps FunCallE{fname} = quantumMaxQueryCostF eps fname
-- known cost formulas
quantumMaxQueryCostE eps PrimCallE{prim} =
  quantumMaxQueryCostPrimitive eps prim
-- basic expressions
quantumMaxQueryCostE _ BasicExprE{basic_expr} = return $ C.callExpr C.Classical basic_expr
quantumMaxQueryCostE _ RandomSampleE{distr_expr} = return $ C.callDistrExpr C.Classical distr_expr
quantumMaxQueryCostE _ _ = error "TODO"

quantumMaxQueryCostS ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumMaxCostablePrimitive primsT sizeT precT
  , m ~ QuantumMaxCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | statement @S@
  Stmt primsT sizeT ->
  m costT
quantumMaxQueryCostS eps ExprS{expr} = quantumMaxQueryCostE eps expr
quantumMaxQueryCostS eps IfThenElseS{s_true, s_false} =
  max <$> quantumMaxQueryCostS eps s_true <*> quantumMaxQueryCostS eps s_false
quantumMaxQueryCostS eps (SeqS ss) = do
  eps_each <- splitEps eps ss
  cs <- zipWithM quantumMaxQueryCostS eps_each ss
  return $ Alg.sum cs

quantumMaxQueryCostF ::
  forall primsT sizeT precT costT m.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumMaxCostablePrimitive primsT sizeT precT
  , m ~ QuantumMaxCostCalculator primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | function name
  Ident ->
  m costT
quantumMaxQueryCostF eps fname = do
  FunDef{mbody} <- view $ _funCtx . Ctx.at fname . singular _Just
  case mbody of
    Nothing -> return $ C.query C.Classical fname -- query an external function
    Just FunBody{body_stmt} -> quantumMaxQueryCostS eps body_stmt -- def: compute using body

quantumMaxQueryCost ::
  forall primsT sizeT precT costT.
  ( Num sizeT
  , Ord precT
  , Floating precT
  , QuantumMaxCostablePrimitive primsT sizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , SizeToPrec sizeT precT
  , Ord costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | failure probability `eps`
  precT ->
  -- | program `P`
  Program primsT sizeT ->
  costT
quantumMaxQueryCost strat a_eps (Program fs) =
  let env =
        default_
          & (_funCtx .~ namedFunsToFunCtx fs)
          & (_precSplitStrat .~ strat)
   in quantumMaxQueryCostF a_eps (fun_name $ last fs)
        & (runReaderT ?? env)
        & (^?! _Just)

-- ================================================================================
-- Quantum Cost
-- ================================================================================

{- | Environment to compute the quantum cost (input dependent)
TODO: rename to something like QuantumExpCostEnv
-}
data QuantumCostEnv primsT sizeT precT
  = QuantumCostEnv
      (QuantumMaxCostEnv primsT sizeT precT)
      (FunInterpCtx sizeT)
  deriving (Generic, HasDefault)

type instance PrimitiveType (QuantumCostEnv primsT sizeT precT) = primsT

type instance SizeType (QuantumCostEnv primsT sizeT precT) = sizeT

type instance PrecType (QuantumCostEnv primsT sizeT precT) = precT

instance HasQuantumMaxCostEnv (QuantumCostEnv primsT sizeT precT) where
  _quantumMaxCostEnv focus (QuantumCostEnv e f) = focus e <&> \e' -> QuantumCostEnv e' f

instance HasFunInterpCtx (QuantumCostEnv primsT sizeT precT) where
  _funInterpCtx focus (QuantumCostEnv e f) = focus f <&> QuantumCostEnv e

instance HasUnitaryCostEnv (QuantumCostEnv primsT sizeT precT) where _unitaryCostEnv = _quantumMaxCostEnv . _unitaryCostEnv

instance HasFunCtx (QuantumCostEnv primsT sizeT precT) where _funCtx = _quantumMaxCostEnv . _funCtx

instance HasPrecisionSplittingStrategy (QuantumCostEnv primsT sizeT precT) where _precSplitStrat = _quantumMaxCostEnv . _precSplitStrat

instance HasEvaluationEnv (QuantumCostEnv primsT sizeT precT) where
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

type QuantumCostCalculator primsT sizeT precT = ReaderT (QuantumCostEnv primsT sizeT precT) Maybe

-- --------------------------------------------------------------------------------
-- Quantum Expected Cost: Primitives
-- --------------------------------------------------------------------------------

-- | Primitives that have a input dependent expected quantum cost
class
  ( QuantumMaxCostablePrimitive primT sizeT precT
  , Evaluatable primT precT
  ) =>
  QuantumCostablePrimitive primT sizeT precT
  where
  quantumQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ QuantumCostCalculator primsT SizeT precT
    , QuantumCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , Prob.RVType precT costT
    , Prob.RVType precT precT
    , SizeToPrec SizeT precT
    , Ord costT
    ) =>
    precT ->
    primT ->
    ProgramState sizeT ->
    m costT
  default quantumQueryCostPrimitive ::
    forall primsT costT m.
    ( m ~ QuantumCostCalculator primsT SizeT precT
    , QuantumCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , Prob.RVType precT costT
    , Prob.RVType precT precT
    , SizeToPrec SizeT precT
    , Ord costT
    , Generic primT
    , GQuantumCostablePrimitive (Rep primT) sizeT precT
    ) =>
    precT ->
    primT ->
    ProgramState sizeT ->
    m costT
  quantumQueryCostPrimitive eps p = gquantumQueryCostPrimitive eps (from p)

instance (Show precT, Fractional precT) => QuantumCostablePrimitive Void sizeT precT where
  quantumQueryCostPrimitive _ = absurd

class GQuantumCostablePrimitive f sizeT precT where
  gquantumQueryCostPrimitive ::
    forall primsT costT m primT.
    ( m ~ QuantumCostCalculator primsT SizeT precT
    , QuantumCostablePrimitive primsT sizeT precT
    , C.CostModel costT
    , precT ~ PrecType costT
    , Prob.RVType precT costT
    , Prob.RVType precT precT
    , SizeToPrec SizeT precT
    , Ord costT
    ) =>
    precT ->
    f primT ->
    ProgramState sizeT ->
    m costT

instance
  (GQuantumCostablePrimitive f1 sizeT precT, GQuantumCostablePrimitive f2 sizeT precT) =>
  GQuantumCostablePrimitive (f1 :+: f2) sizeT precT
  where
  gquantumQueryCostPrimitive eps (L1 p) = gquantumQueryCostPrimitive eps p
  gquantumQueryCostPrimitive eps (R1 p) = gquantumQueryCostPrimitive eps p

instance (GQuantumCostablePrimitive f sizeT precT) => GQuantumCostablePrimitive (M1 i c f) sizeT precT where
  gquantumQueryCostPrimitive eps (M1 x) = gquantumQueryCostPrimitive eps x

instance
  (QuantumCostablePrimitive f sizeT precT) =>
  GQuantumCostablePrimitive (K1 i f) sizeT precT
  where
  gquantumQueryCostPrimitive eps (K1 x) = quantumQueryCostPrimitive eps x

-- --------------------------------------------------------------------------------
-- Quantum Expected Cost: Core Language
-- --------------------------------------------------------------------------------
quantumQueryCostE ::
  forall primsT precT costT m.
  ( Floating precT
  , QuantumCostablePrimitive primsT SizeT precT
  , m ~ QuantumCostCalculator primsT SizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , Prob.RVType precT costT
  , Prob.RVType precT precT
  , SizeToPrec SizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | state \( \sigma \)
  ProgramState SizeT ->
  -- | statement @S@
  Expr primsT SizeT ->
  m costT
quantumQueryCostE eps sigma FunCallE{fname, args} =
  let vs = args <&> \x -> sigma ^. Ctx.at x . non' (error "invalid arg")
   in quantumQueryCostF eps vs fname
-- known cost formulas
quantumQueryCostE eps sigma PrimCallE{prim} = do
  quantumQueryCostPrimitive eps prim sigma
-- basic expressions
quantumQueryCostE _ _ BasicExprE{basic_expr} = return $ C.callExpr C.Classical basic_expr
quantumQueryCostE _ _ RandomSampleE{distr_expr} = return $ C.callDistrExpr C.Classical distr_expr
quantumQueryCostE _ _ _ = error "TODO"

quantumQueryCostS ::
  forall primsT precT costT m.
  ( Floating precT
  , QuantumCostablePrimitive primsT SizeT precT
  , m ~ QuantumCostCalculator primsT SizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , Prob.RVType precT costT
  , Prob.RVType precT precT
  , SizeToPrec SizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | state \( \sigma \)
  ProgramState SizeT ->
  -- | statement @S@
  Stmt primsT SizeT ->
  m costT
quantumQueryCostS eps sigma ExprS{expr} = quantumQueryCostE eps sigma expr
quantumQueryCostS eps sigma IfThenElseS{cond, s_true, s_false} =
  let s = if valueToBool (sigma ^?! Ctx.at cond . singular _Just) then s_true else s_false
   in quantumQueryCostS eps sigma s
quantumQueryCostS eps sigma (SeqS ss) = do
  env <- do
    funCtx <- view _funCtx
    interpCtx <- view _funInterpCtx
    return $
      default_
        & (_funCtx .~ funCtx)
        & (_funInterpCtx .~ interpCtx)

  let stepS s sigma_s =
        execStmt @primsT @precT s
          & (execStateT ?? sigma_s)
          & (runReaderT ?? env)
  eps_each <- splitEps eps ss

  (_, cs) <- forAccumM (pure sigma) (zip ss eps_each) $ \distr (s, eps_s) -> do
    c <- Prob.expectationA (\sigma' -> quantumQueryCostS eps_s sigma' s) distr
    return (distr >>= stepS s, c)

  return $ Alg.sum cs

quantumQueryCostF ::
  forall primsT precT costT m.
  ( Floating precT
  , QuantumCostablePrimitive primsT SizeT precT
  , m ~ QuantumCostCalculator primsT SizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , Prob.RVType precT costT
  , Prob.RVType precT precT
  , SizeToPrec SizeT precT
  , Ord costT
  ) =>
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | inputs
  [Value SizeT] ->
  -- | function name
  Ident ->
  m costT
quantumQueryCostF eps arg_vals fname = do
  view (_funCtx . Ctx.at fname . non' (error "invalid function") . to mbody) >>= \case
    Nothing -> return $ C.query C.Classical fname -- query an external function
    Just FunBody{param_names, body_stmt} -> do
      let omega = Ctx.fromList $ zip param_names arg_vals
      quantumQueryCostS eps omega body_stmt

quantumQueryCost ::
  forall primsT precT costT.
  ( Floating precT
  , QuantumCostablePrimitive primsT SizeT precT
  , C.CostModel costT
  , precT ~ PrecType costT
  , Prob.RVType precT costT
  , Prob.RVType precT precT
  , SizeToPrec SizeT precT
  , Ord costT
  ) =>
  PrecisionSplittingStrategy ->
  -- | failure probability \( \varepsilon \)
  precT ->
  -- | program @P@
  Program primsT SizeT ->
  -- | data Subtypings
  FunInterpCtx SizeT ->
  -- | input
  [Value SizeT] ->
  costT
quantumQueryCost strat a_eps (Program fs) interpCtx inp =
  let env =
        default_
          & (_funCtx .~ namedFunsToFunCtx fs)
          & (_precSplitStrat .~ strat)
          & (_funInterpCtx .~ interpCtx)
   in quantumQueryCostF a_eps inp (fun_name $ last fs)
        & (runReaderT ?? env)
        & (^?! _Just)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.UnitaryCost (
  UnitaryCostPrim (..),
  UnitaryQueries (..),
  strongQueries,
  weakQueries,
  zeroQ,
  totalWeakUnitaryQueries,
) where

import GHC.Generics

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.TypeCheck

-- --------------------------------------------------------------------------------
-- Unitary Compiler: Costs, Error.
-- --------------------------------------------------------------------------------

data UnitaryQueries prec = UnitaryQueries {strong, weak :: prec}
  deriving (Eq, Read, Show)

strongQueries, weakQueries :: (Num prec) => prec -> UnitaryQueries prec
strongQueries q = UnitaryQueries{strong = q, weak = 0}
weakQueries q = UnitaryQueries{strong = 0, weak = q}

zeroQ :: (Num prec) => UnitaryQueries prec
zeroQ = UnitaryQueries{strong = 0, weak = 0}

-- | Total number of queries to a "weak" (i.e. with entangled aux) implementation of the sub-function.
totalWeakUnitaryQueries :: (Num prec) => UnitaryQueries prec -> prec
totalWeakUnitaryQueries UnitaryQueries{strong, weak} = 2 * strong + weak

{- | Unitary query and operation costs of a primitive.
Represents one level of the call graph.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , TypeCheckPrim prim size
  ) =>
  UnitaryCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function.
  unitaryQueryCosts :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)
  default unitaryQueryCosts ::
    ( Generic prim
    , GUnitaryCostPrim (Rep prim) size prec
    ) =>
    prim ->
    A.FailProb prec ->
    PrimFnShape prim (UnitaryQueries prec)
  unitaryQueryCosts prim = reshapeUnsafe . gunitaryQueryCosts (from prim)

  -- | Cost of all additional operations. Defaults to zero.
  unitaryExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  -- unitaryExprCosts _ _ = Alg.zero
  default unitaryExprCosts ::
    ( Generic prim
    , GUnitaryCostPrim (Rep prim) size prec
    , C.CostModel cost
    , precT ~ PrecType cost
    ) =>
    prim ->
    A.FailProb prec ->
    cost
  unitaryExprCosts prim = gunitaryExprCosts (from prim)

class GUnitaryCostPrim f size prec | f -> size prec where
  gunitaryQueryCosts :: f prim -> A.FailProb prec -> [UnitaryQueries prec]
  gunitaryExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => f prim -> A.FailProb prec -> cost

instance (GUnitaryCostPrim a size prec, GUnitaryCostPrim b size prec) => GUnitaryCostPrim (a :+: b) size prec where
  gunitaryQueryCosts (L1 x) = gunitaryQueryCosts x
  gunitaryQueryCosts (R1 x) = gunitaryQueryCosts x

  gunitaryExprCosts (L1 x) = gunitaryExprCosts x
  gunitaryExprCosts (R1 x) = gunitaryExprCosts x

instance (GUnitaryCostPrim f size prec) => GUnitaryCostPrim (M1 i c f) size prec where
  gunitaryQueryCosts (M1 x) = gunitaryQueryCosts x
  gunitaryExprCosts (M1 x) = gunitaryExprCosts x

instance (UnitaryCostPrim a size prec) => GUnitaryCostPrim (K1 i a) size prec where
  gunitaryQueryCosts (K1 x) = shapeToList . unitaryQueryCosts x
  gunitaryExprCosts (K1 x) = unitaryExprCosts x

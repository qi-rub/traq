{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class (
  -- * Primitives
  Primitive (..),
  PrimFnShape,
  ValidPrimShape (..),
  reshapeUnsafe,

  -- ** Typeclasses
  SerializePrim (..),
  TypeCheckPrim (..),
  EvalPrim (..),

  -- ** Unitary Compilation and Cost
  UnitaryCostPrim (..),
  UnitaryQueries (..),
  strongQueries,
  weakQueries,
  zeroQ,

  -- ** Classical-Quantum Compilation and Cost
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),

  -- ** Partial Functions
  PartialFun (..),
  placeArgs,
) where

-- Re-export all modules

import Traq.Primitives.Class.Cost
import Traq.Primitives.Class.Eval
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.Serialize
import Traq.Primitives.Class.TypeCheck

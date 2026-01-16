{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Quantum Max Finding.

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module Traq.Primitives.Max.QMax (
  -- * Primitive
  QMax (..),

  -- * Formulas
  _EQMax,
  _WQMax,
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Text.Printf (printf)

import qualified Numeric.Algebra as Alg

import qualified Traq.Analysis as P
import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- [1], Page 16, below Eq. 11
_EQMax :: forall sizeT precT. (Floating precT, P.SizeToPrec sizeT precT) => sizeT -> precT
_EQMax n = 6.3505 * sqrt_n + 2.8203
 where
  sqrt_n :: precT
  sqrt_n = sqrt $ P.sizeToPrec n

-- [1], Corollary 1.
_WQMax :: forall sizeT precT. (Floating precT, P.SizeToPrec sizeT precT) => sizeT -> P.FailProb precT -> precT
_WQMax n eps = 3 * _EQMax n * log_eps
 where
  log_eps :: precT
  log_eps = log (1 / P.getFailProb eps)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax sizeT precT = QMax {arg_ty :: P.VarType sizeT}
  deriving (Eq, Show, Read)

type instance SizeType (QMax sizeT precT) = sizeT
type instance PrecType (QMax sizeT precT) = precT

newtype QMaxFunArg a = QMaxFunArg {fun :: a}

type instance PrimFnShape (QMax size prec) = QMaxFunArg

instance ValidPrimShape QMaxFunArg where
  listToShape [fun] = Right QMaxFunArg{fun}
  listToShape _ = Left "max expects exactly one function argument"

  shapeToList QMaxFunArg{fun} = [fun]

instance (Show sizeT) => SerializePrim (QMax sizeT precT) where
  primNames = ["max"]
  parsePrimParams tp _ = QMax <$> P.varType tp
  printPrimParams QMax{arg_ty} = [show arg_ty]

-- Type check
instance (Eq sizeT) => TypeCheckPrim (QMax sizeT precT) sizeT where
  inferRetTypesPrim QMax{arg_ty} QMaxFunArg{fun = fun_type} = do
    let P.FnType param_types ret_types = fun_type

    when (param_types /= [arg_ty]) $
      throwError "max: argument does not match specified type."

    case ret_types of
      [P.Fin _] -> pure ()
      _ ->
        throwError $
          printf "`max` fun arg must return a single value, got %d values" (length ret_types)

    return ret_types

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance EvalPrim (QMax sizeT precT) sizeT precT where
  evalPrim QMax{arg_ty} QMaxFunArg{fun = fun_eval} = do
    let search_range = P.domain arg_ty

    vs <- forM search_range $ \val -> do
      res <- fun_eval [val]
      case res of
        [P.FinV v] -> return v
        _ -> error "fail"

    return [P.FinV $ maximum vs]

-- ================================================================================
-- Abstract Costs
-- ================================================================================

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating precT
  , P.SizeToPrec sizeT precT
  ) =>
  UnitaryCostPrim (QMax sizeT precT) sizeT precT
  where
  unitaryQueryCosts QMax{arg_ty} eps = QMaxFunArg{fun = strongQueries $ _WQMax _N eps}
   where
    _N = P.domainSize arg_ty

  unitaryExprCosts _ _ = Alg.zero

instance
  ( Integral sizeT
  , Floating precT
  , P.SizeToPrec sizeT precT
  ) =>
  QuantumHavocCostPrim (QMax sizeT precT) sizeT precT
  where
  quantumQueryCostsUnitary QMax{arg_ty} eps = QMaxFunArg{fun = strongQueries $ _WQMax _N eps}
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumQueryCostsQuantum _ _ = QMaxFunArg{fun = 0}

  quantumExprCosts = Alg.zero

instance
  ( Floating precT
  , Integral sizeT
  , P.SizeToPrec sizeT precT
  ) =>
  QuantumExpCostPrim (QMax sizeT precT) sizeT precT
  where
  quantumExpQueryCostsUnitary QMax{arg_ty} _ _ = QMaxFunArg{fun = strongQueries $ _EQMax _N}
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumExpQueryCostsQuantum _ _ _ = QMaxFunArg{fun = []}

  quantumExpExprCosts = Alg.zero

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

import qualified Traq.Analysis as A
import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax size prec = QMax {arg_ty :: P.VarType size}
  deriving (Eq, Show, Read)

type instance SizeType (QMax size prec) = size
type instance PrecType (QMax size prec) = prec

newtype QMaxFunArg a = QMaxFunArg {fun :: a}

type instance PrimFnShape (QMax size prec) = QMaxFunArg

instance ValidPrimShape QMaxFunArg where
  listToShape [fun] = Right QMaxFunArg{fun}
  listToShape _ = Left "max expects exactly one function argument"

  shapeToList QMaxFunArg{fun} = [fun]

instance P.MapSize (QMax size prec) where
  type MappedSize (QMax size prec) size' = QMax size' prec

  mapSize f (QMax t) = QMax (P.mapSize f t)

instance (Show size) => SerializePrim (QMax size prec) where
  primNames = ["max"]
  parsePrimParams tp _ = QMax <$> P.varType tp
  printPrimParams QMax{arg_ty} = [show arg_ty]

-- Type check
instance (Eq size) => TypeCheckPrim (QMax size prec) size where
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
instance EvalPrim (QMax size prec) size prec where
  evalPrim QMax{arg_ty} QMaxFunArg{fun = fun_eval} = do
    let search_range = P.domain arg_ty

    vs <- forM search_range $ \val -> do
      res <- fun_eval [val]
      case res of
        [P.FinV v] -> return v
        _ -> error "fail"

    return [P.FinV $ maximum vs]

-- ================================================================================
-- Unitary
-- ================================================================================

instance
  (Integral size, Floating prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (QMax size prec) size prec
  where
  unitaryQueryCosts QMax{arg_ty} _ = QMaxFunArg{fun = weakQueries (A.sizeToPrec _N)}
   where
    _N = P.domainSize arg_ty

  unitaryExprCosts _ _ = Alg.zero

instance UnitaryCompilePrim (QMax size prec) size prec where
  compileUPrim QMax{arg_ty} _ = do
    error "TODO: CompileU QMax"

-- ================================================================================
-- Quantum
-- ================================================================================

-- [1], Page 16, below Eq. 11
_EQMax :: forall size prec. (Floating prec, A.SizeToPrec size prec) => size -> prec
_EQMax n = 6.3505 * sqrt_n + 2.8203
 where
  sqrt_n :: prec
  sqrt_n = sqrt $ A.sizeToPrec n

-- [1], Corollary 1.
_WQMax :: forall size prec. (Floating prec, A.SizeToPrec size prec) => size -> A.FailProb prec -> prec
_WQMax n eps = 3 * _EQMax n * log_eps
 where
  log_eps :: prec
  log_eps = log (1 / A.getFailProb eps)

instance
  (Integral size, Floating prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (QMax size prec) size prec
  where
  quantumQueryCostsUnitary QMax{arg_ty} eps = QMaxFunArg{fun = strongQueries $ _WQMax _N eps}
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumQueryCostsQuantum _ _ = QMaxFunArg{fun = 0}

  quantumExprCosts = Alg.zero

instance
  (Floating prec, Integral size, A.SizeToPrec size prec) =>
  QuantumExpCostPrim (QMax size prec) size prec
  where
  quantumExpQueryCostsUnitary QMax{arg_ty} _ _ = QMaxFunArg{fun = strongQueries $ _EQMax _N}
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumExpQueryCostsQuantum _ _ _ = QMaxFunArg{fun = []}

  quantumExpExprCosts = Alg.zero

instance QuantumCompilePrim (QMax size prec) size prec where
  compileQPrim QMax{} eps = do
    error "TODO: CompileQ QMax"

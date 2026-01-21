{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Traq.Analysis.Error.Prelude (
  -- * Failure probability type
  FailProb,
  failProb,
  getFailProb,
  splitFailProb,

  -- ** Conversions
  unitarySubroutineTVError,
  unitarySubroutineTVErrorTotal,
  unitarySubroutineTVBudget,

  -- * Monad
  ErrorAnalysisMonad,
  ErrorReqs,
) where

import Control.Monad.Reader (Reader)

import Traq.Analysis.Prelude (SizeToPrec)
import Traq.ProtoLang.Syntax (FunCtx)

-- ================================================================================
-- Failure Probabilities
-- ================================================================================

-- | Wrapper for values representing a failure probability in [0, 1]
newtype FailProb precT = FailProb precT
  deriving (Eq, Read, Show, Num, Ord)

-- | Safe constructor
failProb :: (Num precT) => precT -> FailProb precT
failProb = FailProb

getFailProb :: FailProb precT -> precT
getFailProb (FailProb p) = p

-- | Helper: split an epsilon into `n` equal parts.
splitFailProb :: (Fractional prec) => FailProb prec -> prec -> FailProb prec
splitFailProb (FailProb p) n = FailProb (p / n)

-- ================================================================================
-- Conversion b/w errors for unitary channels <-> unitary dilations.
-- ================================================================================

{- | Given the TV error of a unitary channel,
convert it to the contribution as a subroutine via the diamond-distance of the unitary dilations.
-}
unitarySubroutineTVError :: (Floating prec) => FailProb prec -> FailProb prec
unitarySubroutineTVError (FailProb eps) = FailProb $ sqrt (2 * eps)

-- | Total error over n calls to subroutine with error eps.
unitarySubroutineTVErrorTotal :: (Floating prec) => prec -> FailProb prec -> FailProb prec
unitarySubroutineTVErrorTotal n eps = FailProb (n * eps')
 where
  FailProb eps' = unitarySubroutineTVError eps

-- | Given a error budget for subroutine, convert to budget for its unitary dilation.
unitarySubroutineTVBudget :: (Fractional prec) => FailProb prec -> FailProb prec
unitarySubroutineTVBudget (FailProb eps) = FailProb $ (eps ^ (2 :: Int)) / 2

-- ================================================================================
-- Context for error analysis
-- ================================================================================

type ErrorReqs size prec =
  ( Floating prec
  , Num size
  , Ord prec
  , SizeToPrec size prec
  )

type ErrorAnalysisMonad ext = Reader (FunCtx ext)

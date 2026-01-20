{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

  -- * Legacy
  L2NormError,
  l2NormError,
  getL2NormError,

  -- ** Conversion
  failProbToNormError,
  normErrorToFailProb,
  requiredFailProbToNormError,
  requiredNormErrorToFailProb,
  DivideError (..),
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

-- ================================================================================
-- Legacy
-- ================================================================================

-- | Wrapper for values representing an l2-norm error in [0, 1]
newtype L2NormError precT = L2NormError precT
  deriving (Eq, Read, Show, Num)
{-# DEPRECATED
  L2NormError
  , l2NormError
  , getL2NormError
  , failProbToNormError
  , normErrorToFailProb
  , requiredFailProbToNormError
  , requiredNormErrorToFailProb
  , DivideError
  "Use FailProb with newer methods"
  #-}

-- | Safe constructor
l2NormError :: (Num precT) => precT -> L2NormError precT
l2NormError = L2NormError

getL2NormError :: L2NormError precT -> precT
getL2NormError (L2NormError p) = p

-- Conversions

{- | Convert a failure prob of a program to a norm error (in a compute-uncompute pattern)
Important: the unitary program will be run twice!
-}
failProbToNormError :: (Floating precT) => FailProb precT -> L2NormError precT
failProbToNormError (FailProb eps) = L2NormError (2 * sqrt eps)

-- | Convert a norm error of a given unitary program to its failure probability (i.e. on measuring output)
normErrorToFailProb :: (Num precT) => L2NormError precT -> FailProb precT
normErrorToFailProb (L2NormError delta) = FailProb (2 * delta)

-- | Compute the maximum norm error allowed to obtain a failure probability of at most eps.
requiredFailProbToNormError :: (Fractional precT) => FailProb precT -> L2NormError precT
requiredFailProbToNormError (FailProb eps) = L2NormError (eps / 2)

{- | Compute the fail prob allowed to obtain a norm-error of at most delta.
Important: the unitary program will be run twice!
-}
requiredNormErrorToFailProb :: (Fractional precT) => L2NormError precT -> FailProb precT
requiredNormErrorToFailProb (L2NormError delta) = FailProb ((delta / 2) ^ (2 :: Int))

class (Fractional prec, Num err) => DivideError err prec | err -> prec where
  divideError :: err -> prec -> err

instance (Fractional prec) => DivideError (FailProb prec) prec where
  divideError (FailProb e) k = FailProb (e / k)

instance (Fractional prec) => DivideError (L2NormError prec) prec where
  divideError (L2NormError e) k = L2NormError (e / k)

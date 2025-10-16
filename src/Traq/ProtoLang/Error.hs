{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Traq.ProtoLang.Error (
  FailProb,
  failProb,
  getFailProb,
  L2NormError,
  l2NormError,
  getL2NormError,

  -- * Conversion
  failProbToNormError,
  normErrorToFailProb,
  requiredFailProbToNormError,
  requiredNormErrorToFailProb,
  DivideError (..),
) where

-- | Wrapper for values representing a failure probability in [0, 1]
newtype FailProb precT = FailProb precT
  deriving (Eq, Read, Show, Num)

-- | Safe constructor
failProb :: (Num precT) => precT -> FailProb precT
failProb = FailProb

getFailProb :: FailProb precT -> precT
getFailProb (FailProb p) = p

-- | Wrapper for values representing an l2-norm error in [0, 1]
newtype L2NormError precT = L2NormError precT
  deriving (Eq, Read, Show, Num)

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

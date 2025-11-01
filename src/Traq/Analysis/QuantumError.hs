{-# LANGUAGE FunctionalDependencies #-}

-- | Error (Failure Probability) analysis for the quantum compiler.
module Traq.Analysis.QuantumError () where

import Traq.Analysis.Error

-- | Failure probability w.r.t. quantum compiler CompileQ.
class FailProbQ ext size prec | ext -> size prec where
  failProbQ :: ext -> FailProb prec

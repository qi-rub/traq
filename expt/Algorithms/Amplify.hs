{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module QCompose.Algorithms.Amplify (amplify, amp1, amplifyRecursive) where

import Control.Monad ((>=>))
import QCompose.Algorithms.Basic (Unitary, LiftedPredicate, reverse_predicate)
import Quipper (QData, reverse_generic_endo, named_gate, gate_Z)

reflect0 :: (QData qa) => Unitary qa
reflect0 = named_gate "Reflect_|0>"

phase_oracle :: (QData q, QData w) => LiftedPredicate q w -> Unitary q
phase_oracle oracle = \q -> do
  (q, w, result) <- oracle q
  result <- gate_Z result 
  q <- reverse_predicate oracle q w result
  return q

amplify :: (QData qa, QData w) => Int -> Unitary qa -> LiftedPredicate qa w -> Unitary qa
amplify 0 alg _ = alg
amplify k alg oracle = amplify (k - 1) alg oracle >=> one_round
  where
    one_round = phase_oracle oracle >=> reverse_generic_endo alg >=> reflect0 >=> alg

amp1 :: (QData qa, QData w) => Unitary qa -> LiftedPredicate qa w -> Unitary qa
amp1 = amplify 1

amplifyRecursive :: (QData qa, QData w) => Int -> Unitary qa -> LiftedPredicate qa w -> Unitary qa
amplifyRecursive 0 alg _ = alg
amplifyRecursive k alg oracle = amp1 (amplifyRecursive (k - 1) alg oracle) oracle


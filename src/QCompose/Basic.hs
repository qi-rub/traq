{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module QCompose.Basic (Unitary, LiftedPredicate, reverse_predicate) where

import Quipper (Circ, Qubit, QData, reverse_generic)

type Unitary a = a -> Circ a

type LiftedFunction a w r = a -> Circ (a, w, r)

type LiftedPredicate a w = LiftedFunction a w Qubit

reverse_predicate :: (QData q, QData w) => LiftedPredicate q w -> q -> w -> Qubit -> Circ q
reverse_predicate oracle q w res = reverse_generic oracle q (q, w, res)
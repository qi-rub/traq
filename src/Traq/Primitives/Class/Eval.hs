{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Eval (
  EvalPrim (..),
) where

import GHC.Generics

import Traq.Prelude
import Traq.Primitives.Class.Prelude
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Evaluation
-- --------------------------------------------------------------------------------

{- | Evaluate a primitive given the semantics of each function argument.
For partial functions, the prefix of arguments are already bound.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  EvalPrim prim size prec
    | prim -> size prec
  where
  evalPrim ::
    forall ext' shape m.
    ( P.Evaluatable ext' size prec
    , m ~ P.Evaluator ext'
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape ([P.Value size] -> m [P.Value size]) ->
    m [P.Value size]
  default evalPrim ::
    forall ext' shape m.
    ( Generic prim
    , GEvalPrim (Rep prim) size prec
    , P.Evaluatable ext' size prec
    , m ~ P.Evaluator ext'
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape ([P.Value size] -> m [P.Value size]) ->
    m [P.Value size]
  evalPrim prim = gevalPrim (from prim) . shapeToList

class GEvalPrim f size prec | f -> size prec where
  gevalPrim ::
    forall ext' m p.
    ( P.Evaluatable ext' size prec
    , m ~ P.Evaluator ext'
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    ) =>
    f p ->
    [[P.Value size] -> m [P.Value size]] ->
    m [P.Value size]

instance (GEvalPrim a size prec, GEvalPrim b size prec) => GEvalPrim (a :+: b) size prec where
  gevalPrim (L1 x) fs = gevalPrim x fs
  gevalPrim (R1 x) fs = gevalPrim x fs

instance (GEvalPrim f size prec) => GEvalPrim (M1 i c f) size prec where
  gevalPrim (M1 x) = gevalPrim x

instance (EvalPrim a size prec) => GEvalPrim (K1 i a) size prec where
  gevalPrim (K1 x) fs = evalPrim x (reshapeUnsafe fs)

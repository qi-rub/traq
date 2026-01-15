{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Eval (
  EvalPrim (..),
) where

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

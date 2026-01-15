{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Eval (
  EvalPrim (..),
) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx

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

instance
  ( EvalPrim prim size prec
  , P.EvalReqs size prec
  ) =>
  P.Evaluatable (Primitive prim) size prec
  where
  eval (Primitive par_funs prim) sigma = do
    fns_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' = P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
      return eval_fn

    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    evalPrim prim shaped_fns_eval

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify (
  -- * Amplify Primitive
  QAmplify (..),
  HasPrimAmplify (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Lens.Micro.GHC (to)
import Lens.Micro.Mtl (view)
import Text.Parsec.Token (GenTokenParser (brackets, float, identifier, symbol))
import Text.Printf (printf)

import Traq.Control.Monad (maybeWithError)
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QAmplify = QAmplify {sampler :: Ident, p_min :: Double}
  deriving (Eq, Show, Read)

{- | Primitive @amplify@ that returns a sample using the sampling function f.
Also returns a boolean flag indicating whether the sample returned is good.
-}
class HasPrimAmplify primT where
  -- | build a call to @amplify@
  mkAmplify :: Ident -> Double -> primT

  -- | extract the sampler.
  getSamplerOfAmplify :: primT -> Ident

  -- | extract the minimum probability
  getPMinOfAmplify :: primT -> Double

instance HasPrimAmplify QAmplify where
  mkAmplify f p = QAmplify{sampler = f, p_min = p}
  getSamplerOfAmplify = sampler
  getPMinOfAmplify = p_min

-- Pretty Printing
instance PP.ToCodeString QAmplify where
  build QAmplify{sampler, p_min} = PP.putWord $ printf "@amplify[%s, %.2f]" sampler p_min

-- Parsing
instance P.CanParsePrimitive QAmplify where
  primitiveParser tp = parseAmplify
   where
    parseAmplify =
      do
        -- Parse "@amplify" keyword
        _ <- symbol tp "@amplify"
        -- Parse the sample and p_min inside brackets
        (sampler, p_min) <- brackets tp $ do
          sampler <- identifier tp
          _ <- symbol tp ","
          p_min <- float tp
          return (sampler, p_min)
        return QAmplify{sampler, p_min}

-- Type check
instance P.TypeCheckablePrimitive QAmplify sizeT where
  typeCheckPrimitive = typeCheckPrimAmplify

typeCheckPrimAmplify ::
  (HasPrimAmplify primT, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  primT ->
  -- | arguments
  [Ident] ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimAmplify prim args =
  do
    let pmin = getPMinOfAmplify prim
    when (pmin < 0 || pmin > 1) $
      throwError $
        printf "pmin must be in [0, 1], got %f" pmin

    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at (getSamplerOfAmplify prim))
        >>= maybeWithError (printf "cannot find sampler function '%s'" (getSamplerOfAmplify prim))

    case ret_types of
      [boolType, _] | boolType == P.tbool -> return ()
      _ -> throwError $ printf "sampler must return (T, Bool), got %s" (show ret_types)

    arg_tys <- mapM Ctx.lookup args
    when (param_types /= arg_tys) $
      throwError
        ( "Invalid arguments to bind to sampler: Params types: "
            ++ show param_types
            ++ ", but arg_tys: "
            ++ show arg_tys
        )

    return [P.tbool, head ret_types]

{- | Evaluate an `amplify` call by evaluating the sampler f to get distribution μ
and get success probability Psucc := P(b=1) conditioned on μ. Finally, returning the distribution
based on Psucc.
-}
instance
  ( Fractional costT
  , Ord costT
  , P.EvaluatablePrimitive primsT primsT costT
  ) =>
  P.EvaluatablePrimitive primsT QAmplify costT
  where
  evalPrimitive = evaluatePrimAmplify

evaluatePrimAmplify ::
  forall primsT primT costT.
  ( Fractional costT
  , HasPrimAmplify primT
  , Ord costT
  , P.EvaluatablePrimitive primsT primsT costT
  ) =>
  primT ->
  [P.Value SizeT] ->
  P.Evaluator primsT SizeT costT [P.Value SizeT]
evaluatePrimAmplify prim arg_vals = do
  let sampler = getSamplerOfAmplify prim
  let p_min = getPMinOfAmplify prim
  sampler_fundef <-
    view $
      P._funCtx
        . Ctx.at sampler
        . to
          (fromMaybe (error "unable to find sampler, please typecheck first!"))
  let sampler_action = P.evalFun @primsT @costT arg_vals sampler sampler_fundef
  eval_env <- view id -- current environment
  let mu = runReaderT sampler_action eval_env -- result distribution
  let p_succ = Prob.probabilityOf predicateBTrue mu
  let p_min' = realToFrac p_min

  lift $
    if
      | p_succ >= p_min' -> Prob.postselect predicateBTrue mu
      | p_succ == 0 -> mu
      | otherwise -> fail "invalid p_min"
 where
  predicateBTrue =
    \case
      (b_val : _) -> P.valueToBool b_val
      _ -> False

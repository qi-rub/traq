{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify (
  -- * Amplify Primitive
  QAmplify (..),
  HasPrimAmplify (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QAmplify = QAmplify {sampler :: Ident, p_min :: Double, sampler_args :: [Ident]}
  deriving (Eq, Show, Read)

{- | Primitive @amplify@ that returns a sample using the sampling function f.
Also returns a boolean flag indicating whether the sample returned is good.
-}
class HasPrimAmplify primT where
  _PrimAmplify :: Traversal' primT QAmplify

  -- | build a call to @amplify@
  mkAmplify :: Ident -> Double -> [Ident] -> primT

instance HasPrimAmplify QAmplify where
  _PrimAmplify = id
  mkAmplify = QAmplify

-- Pretty Printing
instance PP.ToCodeString QAmplify where
  build QAmplify{sampler, p_min, sampler_args} = PP.putWord $ printf "@amplify[%s, %.2f](%s)" sampler p_min (PP.commaList sampler_args)

-- Parsing
instance P.CanParsePrimitive QAmplify where
  primitiveParser TokenParser{..} = parseAmplify
   where
    parseAmplify =
      do
        -- Parse "@amplify" keyword
        _ <- symbol "@amplify"
        -- Parse the sample and p_min inside brackets
        (sampler, p_min) <- brackets $ do
          sampler <- identifier
          comma
          p_min <- float
          return (sampler, p_min)
        sampler_args <- parens $ commaSep identifier
        return QAmplify{sampler, p_min, sampler_args}

-- Type check
instance P.TypeCheckablePrimitive QAmplify where
  typeCheckPrimitive = typeCheckPrimAmplify

typeCheckPrimAmplify ::
  (HasPrimAmplify primT, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  primT ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimAmplify prim = do
  let QAmplify{sampler, p_min, sampler_args} = prim ^?! _PrimAmplify

  when (p_min < 0 || p_min > 1) $
    throwError $
      printf "p_min must be in [0, 1], got %f" p_min

  P.FunDef{P.param_types, P.ret_types} <-
    view (Ctx.at sampler)
      >>= maybeWithError (printf "cannot find sampler function '%s'" sampler)

  sample_ty <-
    case ret_types of
      [boolType, t] | boolType == P.tbool -> return t
      _ -> throwError $ printf "sampler must return (Bool, T), got %s" (show ret_types)

  arg_tys <- mapM Ctx.lookup sampler_args
  when (param_types /= arg_tys) $
    throwError
      ( "Invalid arguments to bind to sampler: expected: "
          ++ show param_types
          ++ ", but got: "
          ++ show arg_tys
      )

  return [P.tbool, sample_ty]

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
  P.ProgramState SizeT ->
  P.Evaluator primsT SizeT costT [P.Value SizeT]
evaluatePrimAmplify prim sigma = do
  let QAmplify{sampler, p_min, sampler_args} = prim ^?! _PrimAmplify

  sampler_fundef <-
    view $
      P._funCtx
        . Ctx.at sampler
        . to (fromMaybe $ error "unable to find sampler, please typecheck first!")

  eval_env <- view id -- current environment
  arg_vals <- runReaderT ?? sigma $ forM sampler_args $ \x -> do
    view $ Ctx.at x . non (error "invalid arg")
  -- result distribution
  let mu =
        P.evalFun @primsT @costT arg_vals (P.NamedFunDef sampler sampler_fundef)
          & (runReaderT ?? eval_env)
  let p_succ = Prob.probabilityOf predicateBTrue mu
  let p_min' = realToFrac p_min

  lift $
    if
      | p_succ >= p_min' -> Prob.postselect predicateBTrue mu
      | p_succ == 0 -> mu
      | otherwise -> Prob.zero
 where
  predicateBTrue =
    \case
      [b_val, _] -> P.valueToBool b_val
      _ -> error "invalid predicate output"

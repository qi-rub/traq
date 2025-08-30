{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify.Prelude (
  -- * Amplify Primitive
  Amplify (..),
  HasPrimAmplify (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
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

{- | Primitive @amplify@ that returns a sample using the sampling function f.
Also returns a boolean flag indicating whether the sample returned is good.
-}
data Amplify = Amplify {sampler :: Ident, p_min :: Double, sampler_args :: [Ident]}
  deriving (Eq, Show, Read)

class HasPrimAmplify primT where
  _PrimAmplify :: Traversal' primT Amplify

  -- | build a call to @amplify@
  mkAmplify :: Amplify -> primT

instance HasPrimAmplify Amplify where
  _PrimAmplify = id
  mkAmplify = id

-- Pretty Printing
instance PP.ToCodeString Amplify where
  build Amplify{sampler, p_min, sampler_args} =
    PP.putWord $ printf "@amplify[%s, %.2f](%s)" sampler p_min (PP.commaList sampler_args)

-- Parsing
instance P.CanParsePrimitive Amplify where
  primitiveParser TokenParser{..} = parseAmplify
   where
    parseAmplify = do
      -- Parse "@amplify" keyword
      _ <- symbol "@amplify"
      -- Parse the sample and p_min inside brackets
      (sampler, p_min) <- brackets $ do
        sampler <- identifier
        comma
        p_min <- float
        return (sampler, p_min)
      sampler_args <- parens $ commaSep identifier
      return Amplify{sampler, p_min, sampler_args}

-- Type check
instance P.HasFreeVars Amplify where
  freeVarsList Amplify{sampler_args} = sampler_args

instance P.TypeCheckablePrimitive Amplify where
  typeCheckPrimitive Amplify{sampler, p_min, sampler_args} = do
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
  P.EvaluatablePrimitive primsT Amplify costT
  where
  evalPrimitive Amplify{sampler, p_min, sampler_args} sigma = do
    sampler_fundef <-
      view $
        P._funCtx
          . Ctx.at sampler
          . non' (error "unable to find sampler, please typecheck first!")

    eval_env <- view id -- current environment
    arg_vals <- runReaderT ?? sigma $ forM sampler_args $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")

    -- result distribution
    let mu =
          P.evalFun @primsT @costT arg_vals (P.NamedFunDef sampler sampler_fundef)
            & (runReaderT ?? eval_env)
    let p_succ = Prob.probabilityOf success mu
    let p_min' = realToFrac p_min

    lift $
      if
        | p_succ >= p_min' -> Prob.postselect success mu
        | p_succ == 0 -> mu
        | otherwise -> Prob.zero
   where
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

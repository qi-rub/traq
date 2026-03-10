{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Primitives.Amplify.Prelude (
  -- * Amplify Primitive
  Amplify (..),
  SamplerFn (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (lift)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Probability as Prob

import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives.Class

{- | Primitive @amplify@ that takes a sampler and returns a good sample w.h.p.
The sampler must return a sample and a boolean flag,
and if there is a good sample, it should return one with probability at least p_min.
-}
data Amplify size prec = Amplify {p_min :: prec}
  deriving (Eq, Show, Read)

type instance SizeType (Amplify size prec) = size
type instance PrecType (Amplify size prec) = prec

newtype SamplerFn a = SamplerFn a

instance ValidPrimShape SamplerFn where
  listToShape [f] = pure $ SamplerFn f
  listToShape _ = throwError "amplify: expected exactly one sampler"
  shapeToList (SamplerFn f) = [f]

type instance PrimFnShape (Amplify size prec) = SamplerFn

instance CPL.MapSize (Amplify size prec) where
  type MappedSize (Amplify size prec) size' = (Amplify size' prec)
  mapSize _ Amplify{..} = Amplify{..}

-- Pretty Printing
instance (Show prec, Fractional prec) => SerializePrim (Amplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp _ = Amplify . realToFrac <$> float tp
  printPrimParams Amplify{p_min} = [show p_min]

-- Type check
instance (CPL.TypingReqs size) => TypeCheckPrim (Amplify size prec) size where
  inferRetTypesPrim _ (SamplerFn sampler_ty) = do
    let CPL.FnType param_types ret_types = sampler_ty

    when (param_types /= []) $ do
      throwError $ printf "amplify: all sampler args must be bound, but got unbound %s" (show param_types)

    sample_ty <-
      case ret_types of
        [boolType, t] | boolType == CPL.tbool -> return t
        _ -> throwError $ printf "amplify: sampler must return (Bool, T), got %s" (show ret_types)

    return [CPL.tbool, sample_ty]

{- | Evaluate an `amplify` call by evaluating the sampler f to get distribution μ
and get success probability Psucc := P(b=1) conditioned on μ. Finally, returning the distribution
based on Psucc.
-}
instance
  (Ord prec, size ~ SizeT, CPL.EvalReqs size prec) =>
  EvalPrim (Amplify size prec) size prec
  where
  evalPrim Amplify{p_min} (SamplerFn sampler) = do
    -- result distribution
    eval_env <- view id
    let mu = sampler [] & runReaderT ?? eval_env

    -- success probability
    let p_succ = Prob.probabilityOf success mu

    lift $
      if
        | p_succ >= p_min -> Prob.postselect success mu
        | p_succ == 0 -> mu
        | otherwise -> Prob.zero
   where
    success [b_val, _] = CPL.valueToBool b_val
    success _ = error "invalid predicate output"

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

import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

{- | Primitive @amplify@ that takes a sampler and returns a good sample w.h.p.
The sampler must return a sample and a boolean flag,
and if there is a good sample, it should return one with probability at least p_min.
-}
data Amplify sizeT precT = Amplify {p_min :: precT}
  deriving (Eq, Show, Read)

type instance SizeType (Amplify sizeT precT) = sizeT
type instance PrecType (Amplify sizeT precT) = precT

newtype SamplerFn a = SamplerFn a

instance ValidPrimShape SamplerFn where
  listToShape [f] = pure $ SamplerFn f
  listToShape _ = throwError "amplify: expected exactly one sampler"
  shapeToList (SamplerFn f) = [f]

type instance PrimFnShape (Amplify size prec) = SamplerFn

instance P.MapSize (Amplify size prec) where
  type MappedSize (Amplify size prec) size' = (Amplify size' prec)
  mapSize _ Amplify{..} = Amplify{..}

-- Pretty Printing
instance (Show prec, Fractional prec) => SerializePrim (Amplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp _ = Amplify . realToFrac <$> float tp
  printPrimParams Amplify{p_min} = [show p_min]

-- Type check
instance (P.TypingReqs sizeT) => TypeCheckPrim (Amplify sizeT precT) sizeT where
  inferRetTypesPrim _ (SamplerFn sampler_ty) = do
    let P.FnType param_types ret_types = sampler_ty

    when (param_types /= []) $ do
      throwError $ printf "amplify: all sampler args must be bound, but got unbound %s" (show param_types)

    sample_ty <-
      case ret_types of
        [boolType, t] | boolType == P.tbool -> return t
        _ -> throwError $ printf "amplify: sampler must return (Bool, T), got %s" (show ret_types)

    return [P.tbool, sample_ty]

{- | Evaluate an `amplify` call by evaluating the sampler f to get distribution μ
and get success probability Psucc := P(b=1) conditioned on μ. Finally, returning the distribution
based on Psucc.
-}
instance
  (Ord precT, sizeT ~ SizeT, P.EvalReqs sizeT precT) =>
  EvalPrim (Amplify sizeT precT) sizeT precT
  where
  evalPrim Amplify{p_min} (SamplerFn sampler) = do
    -- TODO also push this into the primitive class book-keeping.
    eval_env <- view id

    -- result distribution
    let mu = sampler [] & runReaderT ?? eval_env
    let p_succ = Prob.probabilityOf success mu

    lift $
      if
        | p_succ >= p_min -> Prob.postselect success mu
        | p_succ == 0 -> mu
        | otherwise -> Prob.zero
   where
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

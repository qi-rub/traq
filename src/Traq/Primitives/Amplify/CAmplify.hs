{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify.CAmplify () where

import GHC.Generics (Generic)

import Traq.Primitives.Amplify.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- | Classical (probabilistic) bounded repetition.
newtype CAmplify = CAmplify Amplify
  deriving (Eq, Show, Read, Generic)

instance HasPrimAmplify CAmplify where
  _PrimAmplify focus (CAmplify p) = CAmplify <$> focus p
  mkAmplify = CAmplify

-- Inherited instances
instance PP.ToCodeString CAmplify where
  build (CAmplify p) = PP.build p

instance P.CanParsePrimitive CAmplify where
  primitiveParser tp = CAmplify <$> P.primitiveParser tp

instance P.HasFreeVars CAmplify
instance P.TypeCheckablePrimitive CAmplify

instance
  (Fractional costT, Ord costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT CAmplify costT

-- Specific instances

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Traq.Numeric.Floating () where

import Data.Int

import qualified Numeric.Algebra as Alg

instance Alg.Additive Float where a + b = a + b
instance Alg.Additive Double where a + b = a + b

instance Alg.Abelian Float
instance Alg.Abelian Double

instance Alg.LeftModule Float Float where a .* b = a * b
instance Alg.LeftModule Alg.Natural Float where n .* f = fromIntegral n * f
instance Alg.LeftModule Integer Float where n .* f = fromIntegral n * f
instance Alg.LeftModule Int Float where n .* f = fromIntegral n * f
instance Alg.LeftModule Int16 Float where n .* f = fromIntegral n * f
instance Alg.LeftModule Int32 Float where n .* f = fromIntegral n * f
instance Alg.LeftModule Int64 Float where n .* f = fromIntegral n * f

instance Alg.RightModule Float Float where a *. b = a * b
instance Alg.RightModule Alg.Natural Float where f *. n = fromIntegral n * f
instance Alg.RightModule Integer Float where f *. n = fromIntegral n * f
instance Alg.RightModule Int Float where f *. n = fromIntegral n * f
instance Alg.RightModule Int16 Float where f *. n = fromIntegral n * f
instance Alg.RightModule Int32 Float where f *. n = fromIntegral n * f
instance Alg.RightModule Int64 Float where f *. n = fromIntegral n * f

instance Alg.LeftModule Double Double where a .* b = a * b
instance Alg.LeftModule Alg.Natural Double where n .* f = fromIntegral n * f
instance Alg.LeftModule Integer Double where n .* f = fromIntegral n * f
instance Alg.LeftModule Int Double where n .* f = fromIntegral n * f
instance Alg.LeftModule Int16 Double where n .* f = fromIntegral n * f
instance Alg.LeftModule Int32 Double where n .* f = fromIntegral n * f
instance Alg.LeftModule Int64 Double where n .* f = fromIntegral n * f

instance Alg.RightModule Double Double where a *. b = a * b
instance Alg.RightModule Alg.Natural Double where f *. n = fromIntegral n * f
instance Alg.RightModule Integer Double where f *. n = fromIntegral n * f
instance Alg.RightModule Int Double where f *. n = fromIntegral n * f
instance Alg.RightModule Int16 Double where f *. n = fromIntegral n * f
instance Alg.RightModule Int32 Double where f *. n = fromIntegral n * f
instance Alg.RightModule Int64 Double where f *. n = fromIntegral n * f

instance Alg.Monoidal Float where zero = 0.0
instance Alg.Monoidal Double where zero = 0.0

instance Alg.Group Float where (-) = (-); negate = negate; subtract = subtract
instance Alg.Group Double where (-) = (-); negate = negate; subtract = subtract

instance Alg.Multiplicative Float where (*) = (*)
instance Alg.Multiplicative Double where (*) = (*)

instance Alg.Commutative Float
instance Alg.Commutative Double

instance Alg.Unital Float where one = 1.0
instance Alg.Unital Double where one = 1.0

instance Alg.Division Float where recip = recip; (/) = (/); (^) = (^)
instance Alg.Division Double where recip = recip; (/) = (/); (^) = (^)

instance Alg.Semiring Float
instance Alg.Semiring Double

instance Alg.Rng Float
instance Alg.Rng Double

instance Alg.Rig Double where fromNatural = fromIntegral
instance Alg.Rig Float where fromNatural = fromIntegral

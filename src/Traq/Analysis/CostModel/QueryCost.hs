{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.CostModel.QueryCost (
  QueryCost (..),
  totalQueryCost,
  SimpleQueryCost (..),
) where

import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified Numeric.Algebra as Alg

import Traq.Data.Default

import Traq.Analysis.CostModel.Class
import Traq.Prelude

data QueryCost precT = QueryCost
  { uqueries :: Map.Map Ident precT
  , cqueries :: Map.Map Ident precT
  }
  deriving (Eq, Ord, Show, Read, Generic, HasDefault, Functor)

type instance PrecType (QueryCost precT) = precT

type Ticks precT = Map.Map Ident precT

totalQueryCost ::
  forall a.
  (Alg.Rig a) =>
  QueryCost a ->
  -- | cost per unitary query
  Ticks a ->
  -- | cost per classical query
  Ticks a ->
  a
totalQueryCost QueryCost{uqueries, cqueries} uticks cticks =
  costOf uqueries uticks Alg.+ costOf cqueries cticks
 where
  costOf :: Ticks a -> Ticks a -> a
  costOf a b = Map.foldr (Alg.+) Alg.zero (Map.intersectionWith (Alg.*) a b)

instance (Alg.Additive a) => Alg.Additive (QueryCost a) where
  (QueryCost u1 c1) + (QueryCost u2 c2) =
    QueryCost
      (Map.unionWith (Alg.+) u1 u2)
      (Map.unionWith (Alg.+) c1 c2)

instance (Alg.Semiring a) => Alg.LeftModule a (QueryCost a) where
  s .* c = fmap (s Alg.*) c

instance (Alg.Semiring a) => Alg.RightModule a (QueryCost a) where
  c *. s = fmap (s Alg.*) c

instance (Alg.Monoidal a) => Alg.LeftModule Alg.Natural (QueryCost a) where
  n .* c = fmap (Alg.sinnum n) c

instance (Alg.Monoidal a) => Alg.RightModule Alg.Natural (QueryCost a) where
  c *. n = n Alg..* c

instance (Alg.Monoidal a) => Alg.Monoidal (QueryCost a) where
  zero = default_

instance (Alg.Rig a) => CostModel (QueryCost a) where
  -- one query each
  query Unitary f = default_{uqueries = Map.singleton f Alg.one}
  query Classical f = default_{cqueries = Map.singleton f Alg.one}

  -- no cost for basic expressions
  callExpr _ _ = default_
  callDistrExpr _ _ = default_

{- | A simple cost that counts the number of queries to all external functions.
It treats unitary and classical queries as the same.
-}
newtype SimpleQueryCost precT = SimpleQueryCost {getCost :: precT}
  deriving (Eq, Ord, Show, Read, Generic, HasDefault, Functor)

type instance PrecType (SimpleQueryCost precT) = precT

instance (Alg.Additive a) => Alg.Additive (SimpleQueryCost a) where
  (SimpleQueryCost a) + (SimpleQueryCost a') = SimpleQueryCost (a Alg.+ a')

instance (Alg.LeftModule r a) => Alg.LeftModule r (SimpleQueryCost a) where
  s .* (SimpleQueryCost c) = SimpleQueryCost $ s Alg..* c

instance (Alg.RightModule r a) => Alg.RightModule r (SimpleQueryCost a) where
  (SimpleQueryCost c) *. s = SimpleQueryCost $ c Alg.*. s

instance (Alg.Monoidal a) => Alg.Monoidal (SimpleQueryCost a) where
  zero = SimpleQueryCost Alg.zero

instance (Alg.Module a a, Alg.Rig a) => CostModel (SimpleQueryCost a) where
  -- one query each
  query _ _ = SimpleQueryCost Alg.one

  -- no cost for basic expressions
  callExpr _ _ = SimpleQueryCost Alg.zero
  callDistrExpr _ _ = SimpleQueryCost Alg.zero

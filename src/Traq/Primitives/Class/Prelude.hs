{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Prelude (
  -- * Primitives
  Primitive (..),
  PrimFnShape,
  ValidPrimShape (..),
  reshapeUnsafe,

  -- ** Partial Functions
  PartialFun (..),
  placeArgs,
) where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec.Token
import Text.Printf (printf)

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

{- | A partially applied function
Syntax: @f(a_1, ..., a_n)@ where each @a_i@ is either an identifier, or a blank @_@
-}
data PartialFun = PartialFun
  { pfun_name :: Ident
  , pfun_args :: [Maybe Ident]
  }
  deriving (Eq, Show)

instance PP.ToCodeString PartialFun where
  build PartialFun{pfun_name, pfun_args} = do
    let args = PP.commaList $ map (fromMaybe "_") pfun_args
    PP.putWord $ printf "%s(%s)" pfun_name args

instance P.Parseable PartialFun where
  parseE TokenParser{..} = do
    pfun_name <- identifier
    pfun_args <- parens $ commaSep ((Nothing <$ symbol "_") <|> (Just <$> identifier))
    return PartialFun{..}

instance P.RenameVars PartialFun where
  renameVars pref f@PartialFun{pfun_args} = f{pfun_args = map (fmap (P.addOnePrefix pref)) pfun_args}

{- | Place a list of concrete values inside a list of incomplete values.
For example, @placeArgs [Just 0, Nothing, Just 2, Nothing] [1, 3] = [0,1,2,3]@
-}
placeArgs :: [Maybe a] -> [a] -> [a]
placeArgs [] [] = []
placeArgs (Just x : xs) as = x : placeArgs xs as
placeArgs (Nothing : xs) (a : as) = a : placeArgs xs as
placeArgs mxs ys = error $ printf "invalid use of placeArgs(%d, %d)" (length mxs) (length ys)

{- | A generic second-order primitive.
It accepts a sequence of partially applied functions.

Authors are provided with simpler type classes to implement the features:
typing, semantics and query costs.
-}
data Primitive prim = Primitive [PartialFun] prim
  deriving (Eq, Show)

type instance SizeType (Primitive p) = SizeType p
type instance PrecType (Primitive p) = PrecType p

instance P.RenameVars (Primitive p) where
  renameVars pref (Primitive par_funs p) = Primitive (map (P.renameVars pref) par_funs) p

{- | The shape of the function arguments that primitive @prim@ expects.
The type @PrimFnShape prim a@ should be a subtype of @[a]@, for every @a@.
This type is useful to specify record constructor names for readability.
-}
type family PrimFnShape prim :: Type -> Type

-- | Conversion between shaped information and a list.
class ValidPrimShape shape where
  listToShape :: [a] -> Either String (shape a)
  shapeToList :: shape a -> [a]

reshapeUnsafe :: (ValidPrimShape shape, ValidPrimShape shape') => shape a -> shape' a
reshapeUnsafe = either (error "please typecheck first") id . listToShape . shapeToList

instance ValidPrimShape [] where
  listToShape = Right
  shapeToList = id

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance (P.MapSize prim) => P.MapSize (Primitive prim) where
  type MappedSize (Primitive prim) size' = Primitive (P.MappedSize prim size')
  mapSize f (Primitive par_funs prim) = Primitive par_funs (P.mapSize f prim)

instance P.HasFreeVars (Primitive prim) where
  freeVarsList (Primitive par_funs _) = concatMap (catMaybes . pfun_args) par_funs

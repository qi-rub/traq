{-# LANGUAGE RecordWildCards #-}

module Traq.Primitives.Class.Prelude (
  -- * Primitives
  PrimFnShape,
  ValidPrimShape (..),
  reshape,
  reshapeUnsafe,

  -- ** Partial Functions
  PartialFun (..),
  placeArgs,
  placeArgsWithExcess,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
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
    pfun_args <- parens $ commaSep (Nothing <$ symbol "_" <|> Just <$> identifier)
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

-- | Place arguments, with the excess at tail position.
placeArgsWithExcess :: [Maybe a] -> [a] -> [a]
placeArgsWithExcess [] as = as
placeArgsWithExcess (Just x : xs) as = x : placeArgsWithExcess xs as
placeArgsWithExcess (Nothing : xs) (a : as) = a : placeArgsWithExcess xs as
placeArgsWithExcess mxs [] = error $ printf "invalid use of placeArgsWithExcess(%d)" (length mxs)

{- | The shape of the function arguments that primitive @prim@ expects.
The type @PrimFnShape prim a@ should be a subtype of @[a]@, for every @a@.
This type is useful to specify record constructor names for readability.
-}
type family PrimFnShape prim :: Type -> Type

-- | Conversion between shaped information and a list.
class ValidPrimShape shape where
  listToShape :: [a] -> Either String (shape a)
  shapeToList :: shape a -> [a]

reshape :: (ValidPrimShape shape, ValidPrimShape shape') => shape a -> Either String (shape' a)
reshape = listToShape . shapeToList

reshapeUnsafe :: (ValidPrimShape shape, ValidPrimShape shape') => shape a -> shape' a
reshapeUnsafe = either (error "please typecheck first") id . reshape

instance ValidPrimShape [] where
  listToShape = Right
  shapeToList = id

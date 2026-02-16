{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Primitives.Simons.Prelude (
  -- * Primitive
  FindXorPeriod (..),
  FindXorPeriodArg (..),
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

{- | Primitive to solve Simon's problem.
Simon's Problem: Given a function @f : {0, 1}^n -> {0, 1}^n@, find an @s : {0, 1}^n@ such that @f(x) = f(x ^ s)@ for every @x@.

Promise of default variant: Either
  (1) @f@ is two-to-one and there is exactly one such @s@, or
  (2) @f@ is one-one.

Promise of Robust variant (used in the primitive below):
  @f@ is allowed to have at most @p_0@ fraction of spurious collisions (i.e. @f(x) = f(x')@ and @x ^ x' /= s@).

References:
  "On the Power of Quantum Computation"
  https://epubs.siam.org/doi/10.1137/S0097539796298637
-}
data FindXorPeriod sizeT precT = FindXorPeriod {n :: sizeT, p_0 :: precT}
  deriving (Eq, Show, Read)

type instance SizeType (FindXorPeriod sizeT precT) = sizeT
type instance PrecType (FindXorPeriod sizeT precT) = precT

newtype FindXorPeriodArg a = FindXorPeriodArg {fun :: a}
  deriving (Eq, Show)

type instance PrimFnShape (FindXorPeriod sizeT precT) = FindXorPeriodArg

instance ValidPrimShape FindXorPeriodArg where
  listToShape [fun] = Right FindXorPeriodArg{fun}
  listToShape _ = Left "FindXorPeriod expects exactly one function"

  shapeToList FindXorPeriodArg{fun} = [fun]

instance P.MapSize (FindXorPeriod size prec) where
  type MappedSize (FindXorPeriod size prec) size' = (FindXorPeriod size' prec)
  mapSize f FindXorPeriod{n, ..} = FindXorPeriod{n = f n, ..}

-- Pretty Printing
instance (Show sizeT) => SerializePrim (FindXorPeriod sizeT Double) where
  primNames = ["findXorPeriod"]
  parsePrimParams TokenParser{..} _ = do
    n <- (Sym.con . fromInteger <$> integer) <|> (Sym.var <$> identifier)
    comma
    p_0 <- float
    return FindXorPeriod{n, p_0}
  printPrimParams FindXorPeriod{n, p_0} = [show n, printf "%.2f" p_0]

-- | Bitsize
bitsize :: (P.TypingReqs size) => P.VarType size -> Maybe size
bitsize (P.Fin 2) = Just 1
bitsize (P.Fin _) = Nothing
bitsize (P.Bitvec n) = Just n
bitsize (P.Arr n t) = (n *) <$> bitsize t
bitsize (P.Tup ts) = sum <$> mapM bitsize ts

instance
  (P.TypingReqs sizeT, Num precT, Ord precT, Show precT) =>
  TypeCheckPrim (FindXorPeriod sizeT precT) sizeT
  where
  inferRetTypesPrim FindXorPeriod{n, p_0} FindXorPeriodArg{fun} = do
    when (p_0 < 0 || p_0 > 1) $
      throwError $
        printf "p_0 must be in [0, 1], got %f" (show p_0)

    let P.FnType param_types ret_types = fun

    when (ret_types /= param_types) $
      throwError $
        printf "FindPeriod: fn must be of type `t -> t`, got output %s -> %s" (show param_types) (show ret_types)

    when (bitsize (P.Tup param_types) /= Just n) $ do
      throwError $
        printf "FindPeriod: mistmatched bitsize: expected %s, got %s" (show n) (show param_types)

    return ret_types

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Primitives.Simons.Prelude (
  -- * Primitive
  FindXorPeriod (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

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
data FindXorPeriod sizeT precT = FindXorPeriod {fun :: Ident, p_0 :: precT, fun_args :: [Ident]}
  deriving (Eq, Show, Read)

type instance SizeType (FindXorPeriod sizeT precT) = sizeT
type instance PrecType (FindXorPeriod sizeT precT) = precT

instance P.MapSize (FindXorPeriod size prec) where
  type MappedSize (FindXorPeriod size prec) size' = (FindXorPeriod size' prec)
  mapSize _ FindXorPeriod{..} = FindXorPeriod{..}

-- Pretty Printing
instance PP.ToCodeString (FindXorPeriod sizeT Double) where
  build FindXorPeriod{fun, p_0, fun_args} =
    PP.putWord $ printf "@findXorPeriod[%s, %.2f](%s)" fun p_0 (PP.commaList fun_args)

-- Parsing
instance (precT ~ Double) => P.Parseable (FindXorPeriod sizeT precT) where
  parseE TokenParser{..} = parseFindXorPeriod
   where
    parseFindXorPeriod = do
      -- Parse "@findXorPeriod" keyword
      _ <- symbol "@findXorPeriod"
      -- Parse the function and p_0 inside brackets
      (fun, p_0) <- brackets $ do
        function <- identifier
        comma
        p_0 <- float
        return (function, p_0)
      fun_args <- parens $ commaSep identifier
      return FindXorPeriod{fun, p_0, fun_args}

instance P.HasFreeVars (FindXorPeriod s precT) where
  freeVarsList FindXorPeriod{fun_args} = fun_args

-- Type check
instance
  ( Show precT
  , Ord precT
  , Num precT
  , P.TypeCheckable sizeT
  ) =>
  P.TypeCheckablePrimitive (FindXorPeriod sizeT precT) sizeT
  where
  typeCheckPrimitive FindXorPeriod{fun, p_0, fun_args} = do
    when (p_0 < 0 || p_0 > 1) $
      throwError $
        printf "p_0 must be in [0, 1], got %f" (show p_0)

    -- Lookup the boolean function
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at fun)
        >>= maybeWithError (printf "cannot find boolean function '%s'" fun)

    -- Expect function of type Fin<N> -> Fin<N>
    param_type <-
      case last param_types of
        P.Fin n -> return (P.Fin n)
        _ ->
          throwError $
            printf
              "function must accept only one finite integer type, e.g. Fin<N>, got %s"
              (show param_types)

    ret_type <-
      case last ret_types of
        P.Fin n -> return (P.Fin n)
        _ ->
          throwError $
            printf
              "function must return only one finite integer type, e.g. Fin<M>, got %s"
              (show ret_types)

    when (param_type /= ret_type) $
      throwError $
        printf
          "input and output type of the function should be the same, got %s vs %s"
          (show param_type)
          (show ret_type)

    -- Check argument types match the function parameters
    arg_tys <- mapM Ctx.lookup fun_args
    let n_args = length arg_tys
    when (take n_args param_types /= arg_tys) $
      throwError $
        "Invalid arguments to bind to function: expected "
          ++ show param_types
          ++ ", but got "
          ++ show arg_tys

    return [ret_type]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Amplify (
  -- * Amplify Primitive
  QAmplify (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Lens.Micro.Mtl (view)
import Text.Parsec.Token (GenTokenParser (brackets, float, identifier, symbol))
import Text.Printf (printf)
import Traq.Control.Monad (maybeWithError)
import qualified Traq.Data.Context as Ctx
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QAmplify = QAmplify {sampler :: Ident, p_min :: Double}
  deriving (Eq, Show, Read)

{- | Primitive @amplify@ that returns a sample using the sampling function f.
Also returns a boolean flag indicating whether the sample returned is good.
-}
class HasPrimAmplify primT where
  -- | build a call to @amplify@
  mkAmplify :: Ident -> Double -> primT

  -- | extract the sampler.
  getSamplerOfAmplify :: primT -> Ident

  -- | extract the minimum probability
  getPMinOfAmplify :: primT -> Double

instance HasPrimAmplify QAmplify where
  mkAmplify f p = QAmplify{sampler = f, p_min = p}
  getSamplerOfAmplify = sampler
  getPMinOfAmplify = p_min

-- Pretty Printing
instance PP.ToCodeString QAmplify where
  build QAmplify{sampler, p_min} = PP.putWord $ printf "@amplify[%s, %.2f]" sampler p_min

-- Parsing
instance P.CanParsePrimitive QAmplify where
  primitiveParser tp = parseAmplify
   where
    parseAmplify =
      do
        -- Parse "@amplify" keyword
        _ <- symbol tp "@amplify"
        -- Parse the sample and p_min inside brackets
        (sampler, p_min) <- brackets tp $ do
          sampler <- identifier tp
          _ <- symbol tp ","
          p_min <- float tp
          return (sampler, p_min)
        return QAmplify{sampler, p_min}

-- Type check
instance P.TypeCheckablePrimitive QAmplify sizeT where
  typeCheckPrimitive = typeCheckPrimAmplify

typeCheckPrimAmplify ::
  (HasPrimAmplify primT, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  primT ->
  -- | arguments
  [Ident] ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimAmplify prim args =
  do
    let pmin = getPMinOfAmplify prim
    when (pmin < 0 || pmin > 1) $
      throwError $
        printf "pmin must be in [0, 1], got %f" pmin

    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at (getSamplerOfAmplify prim))
        >>= maybeWithError (printf "cannot find sampler function '%s'" (getSamplerOfAmplify prim))

    case ret_types of
      [boolType, _] | boolType == P.tbool -> return ()
      _ -> throwError $ printf "sampler must return (T, Bool), got %s" (show ret_types)

    arg_tys <- mapM Ctx.lookup args
    when (param_types /= arg_tys) $
      throwError
        ( "Invalid arguments to bind to sampler: Params types: "
            ++ show param_types
            ++ ", but arg_tys: "
            ++ show arg_tys
        )

    return [P.tbool, head ret_types]

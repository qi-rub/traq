{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Quantum Counting

Reference: https://arxiv.org/pdf/quant-ph/9805082
-}
module QCompose.Primitives.Search.QCount () where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

newtype QCount = QCount {predicate :: Ident}
  deriving (Eq, Show, Read)

instance ToCodeString QCount where
  toCodeString QCount{predicate} = printf "@count[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QCount where
  primitiveParser tp = do
    symbol tp "@count"
    predicate <- brackets tp $ identifier tp
    return QCount{predicate}

-- Type check
instance P.TypeCheckablePrimitive QCount sizeT where
  typeCheckPrimitive QCount{predicate} args = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find `count` predicate `%s`" predicate)

    when (length ret_types /= 1) $
      throwError $
        printf "`count` predicate must return a single value, got %d" (length ret_types)

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    let P.Fin n = last param_types

    return [P.Fin (n + 1)]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT QCount
  where
  evalPrimitive QCount{predicate} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    values <- forM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res

    let n_sol = values & filter (/= 0) & length & fromIntegral

    return [n_sol]

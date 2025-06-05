module QCompose.Primitives.Search.Prelude (
  HasPrimAny (..),
  HasPrimSearch (..),

  -- * Boilerplate functions

  -- ** Parsing
  parsePrimWithPredicates,
  parsePrimAny,
  parsePrimSearch,

  -- ** Typecheck
  typeCheckSearchPredicate,
  typeCheckPrimAny,
  typeCheckPrimSearch,
) where

import Control.Monad (replicateM, when)
import Control.Monad.Except (throwError)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import qualified QCompose.ProtoLang as P

-- | Primitive @any@ that returns True if there is a solution to the predicate.
class HasPrimAny primT where
  -- | build a call to @any@
  mkAny :: Ident -> primT

  -- | extract the predicate.
  getPredicateOfAny :: primT -> Ident

{- | Primitive @search@ that returns a solution to the predicate if exists, otherwise returns a random value.
 Also returns a bit indicating if a solution was found.
-}
class HasPrimSearch primT where
  -- | build a call to @search@
  mkSearch :: Ident -> primT

  -- | extract the predicate.
  getPredicateOfSearch :: primT -> Ident

-- ================================================================================
-- Parsing/Printing
-- ================================================================================

{- | Parse a primitive with a list of predicate names.
 Example: @parsePrimWithPredicates "any" 1@ can be used to implement the parser for @any@.
-}
parsePrimWithPredicates :: String -> Int -> TokenParser () -> Parser [String]
parsePrimWithPredicates prim_name n_preds tp = do
  _ <- symbol tp $ "@" ++ prim_name
  replicateM n_preds $ brackets tp $ identifier tp

parsePrimAny :: (HasPrimAny primT) => String -> TokenParser () -> Parser primT
parsePrimAny name tp = do
  [predicate] <- parsePrimWithPredicates name 1 tp
  return $ mkAny predicate

parsePrimSearch :: (HasPrimSearch primT) => String -> TokenParser () -> Parser primT
parsePrimSearch name tp = do
  [predicate] <- parsePrimWithPredicates name 1 tp
  return $ mkSearch predicate

-- ================================================================================
-- Typecheck
-- ================================================================================

typeCheckSearchPredicate ::
  (P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  Ident ->
  -- | arguments
  [Ident] ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckSearchPredicate predicate args = do
  P.FunDef{P.param_types, P.ret_types} <-
    view (Ctx.at predicate)
      >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

  when (ret_types /= [P.tbool]) $
    throwError "search predicate must return a single Bool"

  arg_tys <- mapM Ctx.lookup args
  let n_args = length arg_tys
  when (take n_args param_types /= arg_tys) $
    throwError "Invalid arguments to bind to predicate"

  return $ drop n_args param_types

typeCheckPrimAny ::
  (HasPrimAny primT, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  primT ->
  -- | arguments
  [Ident] ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimAny prim args = do
  typeCheckSearchPredicate (getPredicateOfAny prim) args
  return [P.tbool]

typeCheckPrimSearch ::
  (HasPrimAny primT, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  primT ->
  -- | arguments
  [Ident] ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimSearch prim args = do
  s_tys <- typeCheckSearchPredicate (getPredicateOfAny prim) args
  return $ P.tbool : s_tys

-- ================================================================================
-- Evaluation
-- ================================================================================

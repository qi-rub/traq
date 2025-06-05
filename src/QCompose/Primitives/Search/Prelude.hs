module QCompose.Primitives.Search.Prelude (
  -- * Generic Search-like primitives
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

  -- ** Evaluation
  runSearchPredicateOnAllInputs,
  hasSolution,
  countSolutions,
  getSearchOutputs,
  evaluatePrimAny,
  evaluatePrimSearch,
  evaluatePrimCount,
) where

import Control.Monad (forM, replicateM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

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

type SearchResult = ([Value], Bool)

isSolution :: SearchResult -> Bool
isSolution = (True ==) . snd

type SearchResults = [SearchResult]

hasSolution :: SearchResults -> Bool
hasSolution = any isSolution

countSolutions :: SearchResults -> Int
countSolutions = length . filter isSolution

getSearchOutputs :: SearchResults -> [[Value]]
getSearchOutputs rets | hasSolution rets = rets & filter isSolution & map fst
getSearchOutputs rets = map fst rets

-- | Run the predicate on each input, and return the input along with the result.
runSearchPredicateOnAllInputs ::
  (P.EvaluatablePrimitive primsT primsT) =>
  Ident ->
  [Value] ->
  P.Evaluator primsT SizeT SearchResults
runSearchPredicateOnAllInputs predicate arg_vals = do
  pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
  let n_fixed_args = length arg_vals

  let search_tys = pred_fun ^. to P.param_types . to (drop n_fixed_args)
  let search_range = mapM P.range search_tys

  forM search_range $ \s_vals -> do
    rets <- P.evalFun (arg_vals ++ s_vals) pred_fun
    return (s_vals, head rets /= 0)

evaluatePrimAny ::
  (P.EvaluatablePrimitive primsT primsT) =>
  Ident ->
  [Value] ->
  P.Evaluator primsT SizeT [Value]
evaluatePrimAny predicate arg_vals = do
  res <- runSearchPredicateOnAllInputs predicate arg_vals
  return [P.boolToValue $ hasSolution res]

evaluatePrimSearch ::
  (P.EvaluatablePrimitive primsT primsT) =>
  Ident ->
  [Value] ->
  P.Evaluator primsT SizeT [Value]
evaluatePrimSearch predicate arg_vals = do
  res <- runSearchPredicateOnAllInputs predicate arg_vals
  let ok = P.boolToValue $ hasSolution res
  let outs = getSearchOutputs res
  lift $ Tree.choice [pure (ok : v) | v <- outs]

evaluatePrimCount ::
  (P.EvaluatablePrimitive primsT primsT) =>
  Ident ->
  [Value] ->
  P.Evaluator primsT SizeT [Value]
evaluatePrimCount predicate arg_vals = do
  res <- runSearchPredicateOnAllInputs predicate arg_vals
  return [fromIntegral $ countSolutions res]

module Traq.Primitives.Search.Prelude (
  -- * Generic Search-like primitives
  HasPrimAny (..),
  HasPrimSearch (..),
  _predicateOfSearch,
  _argsOfSearchPred,

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
import Control.Monad.Reader (ReaderT (..))
import Data.Maybe (fromMaybe)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Search primitive: (predicate, prefix of arguments to bind)
type PrimSearchInfo = (Ident, [Ident])

-- | Primitive @any@ that returns True if there is a solution to the predicate.
class HasPrimAny primT where
  -- | build a call to @any@, given the predicate and args.
  mkPrimAny :: Ident -> [Ident] -> primT

  -- | Prism to access the primitive.
  _PrimAny :: Traversal' primT PrimSearchInfo

{- | Primitive @search@ that returns a solution to the predicate if exists, otherwise returns a random value.
 Also returns a bit indicating if a solution was found.
-}
class HasPrimSearch primT where
  -- | build a call to @any@, given the predicate and args.
  mkPrimSearch :: Ident -> [Ident] -> primT

  -- | Prism to access the primitive.
  _PrimSearch :: Traversal' primT PrimSearchInfo

_predicateOfSearch :: Lens' PrimSearchInfo Ident
_predicateOfSearch = _1

_argsOfSearchPred :: Lens' PrimSearchInfo [Ident]
_argsOfSearchPred = _2

-- ================================================================================
-- Parsing/Printing
-- ================================================================================

{- | Parse a primitive with a list of predicate names, each bound to a set of arguments.
 Example: @parsePrimWithPredicates "any" 1@ can be used to implement the parser for @any@.
-}
parsePrimWithPredicates :: Ident -> Int -> TokenParser () -> Parser [(Ident, [Ident])]
parsePrimWithPredicates prim_name n_preds tp = do
  _ <- symbol tp $ "@" ++ prim_name
  replicateM n_preds $ do
    predicate <- brackets tp $ identifier tp
    args <- parens tp $ commaSep tp $ identifier tp
    pure (predicate, args)

parsePrimAny :: (HasPrimAny primT) => String -> TokenParser () -> Parser primT
parsePrimAny name tp = do
  [(predicate, args)] <- parsePrimWithPredicates name 1 tp
  return $ mkPrimAny predicate args

parsePrimSearch :: (HasPrimSearch primT) => String -> TokenParser () -> Parser primT
parsePrimSearch name tp = do
  [(predicate, args)] <- parsePrimWithPredicates name 1 tp
  return $ mkPrimSearch predicate args

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
  primT ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimAny prim = do
  let (p, args) = prim ^?! _PrimAny
  typeCheckSearchPredicate p args
  return [P.tbool]

typeCheckPrimSearch ::
  (HasPrimSearch primT, P.TypeCheckable sizeT) =>
  primT ->
  P.TypeChecker primsT sizeT [P.VarType sizeT]
typeCheckPrimSearch prim = do
  let (p, args) = prim ^?! _PrimSearch
  s_tys <- typeCheckSearchPredicate p args
  return $ P.tbool : s_tys

-- ================================================================================
-- Evaluation
-- ================================================================================

type SearchResult = ([P.Value SizeT], Bool)

isSolution :: SearchResult -> Bool
isSolution = (True ==) . snd

type SearchResults = [SearchResult]

hasSolution :: SearchResults -> Bool
hasSolution = any isSolution

countSolutions :: SearchResults -> Int
countSolutions = length . filter isSolution

getSearchOutputs :: SearchResults -> [[P.Value SizeT]]
getSearchOutputs rets | hasSolution rets = rets & filter isSolution & map fst
getSearchOutputs rets = map fst rets

-- | Run the predicate on each input, and return the input along with the result.
runSearchPredicateOnAllInputs ::
  forall primsT costT.
  ( P.EvaluatablePrimitive primsT primsT costT
  , Fractional costT
  ) =>
  Ident ->
  [P.Value SizeT] ->
  P.Evaluator primsT SizeT costT SearchResults
runSearchPredicateOnAllInputs predicate arg_vals = do
  pred_fun <- view $ P._funCtx . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
  let n_fixed_args = length arg_vals

  let search_tys = pred_fun ^. to P.param_types . to (drop n_fixed_args)
  let search_range = mapM P.domain search_tys

  forM search_range $ \s_vals -> do
    rets <- P.evalFun (arg_vals ++ s_vals) predicate pred_fun
    return (s_vals, P.valueToBool $ head rets)

evaluatePrimAny ::
  ( HasPrimAny primT
  , P.EvaluatablePrimitive primsT primsT costT
  , Fractional costT
  ) =>
  primT ->
  P.ProgramState SizeT ->
  P.Evaluator primsT SizeT costT [P.Value SizeT]
evaluatePrimAny prim sigma = do
  let (predicate, args) = prim ^?! _PrimAny

  arg_vals <- runReaderT ?? sigma $ forM args $ \x -> do
    view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

  res <- runSearchPredicateOnAllInputs predicate arg_vals
  return [P.toValue $ hasSolution res]

evaluatePrimSearch ::
  ( HasPrimSearch primT
  , P.EvaluatablePrimitive primsT primsT costT
  , Fractional costT
  ) =>
  primT ->
  P.ProgramState SizeT ->
  P.Evaluator primsT SizeT costT [P.Value SizeT]
evaluatePrimSearch prim sigma = do
  let (predicate, args) = prim ^?! _PrimSearch

  arg_vals <- runReaderT ?? sigma $ forM args $ \x -> do
    view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

  res <- runSearchPredicateOnAllInputs predicate arg_vals
  let ok = P.toValue $ hasSolution res
  let outs = getSearchOutputs res
  Prob.uniform [ok : v | v <- outs]

evaluatePrimCount ::
  ( P.EvaluatablePrimitive primsT primsT costT
  , Fractional costT
  ) =>
  Ident ->
  [Ident] ->
  P.ProgramState SizeT ->
  P.Evaluator primsT SizeT costT [P.Value SizeT]
evaluatePrimCount predicate args sigma = do
  arg_vals <- runReaderT ?? sigma $ forM args $ \x -> do
    view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

  res <- runSearchPredicateOnAllInputs predicate arg_vals
  return [P.FinV $ fromIntegral $ countSolutions res]

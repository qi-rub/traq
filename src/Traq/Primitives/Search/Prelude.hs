{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Primitives.Search.Prelude (
  -- * Basic search primitives
  SearchLikePrim (..),

  -- ** @any@
  PrimAny (..),
  parsePrimAnyWithName,

  -- ** @search@
  PrimSearch (..),
  parsePrimSearchWithName,

  -- ** print
  printSearchLikePrim,

  -- ** typecheck
  typeCheckSearchPredicate,

  -- ** Evaluation
  runSearchPredicateOnAllInputs,
  hasSolution,
  countSolutions,
  getSearchOutputs,
  evaluatePrimCount,
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Writer (MonadWriter)
import Data.Maybe (fromMaybe)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import Traq.Prelude
import Traq.Primitives.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Search-like primitive: has a predicate and a list of arguments to bind to it.
-- ================================================================================
data SearchLikePrim = SearchLikePrim {predicate :: Ident, pred_args :: [Ident]}

printSearchLikePrim ::
  (IsA SearchLikePrim primT, MonadWriter [String] m, MonadFail m) =>
  Ident ->
  primT ->
  m ()
printSearchLikePrim name prim =
  let SearchLikePrim{predicate, pred_args} = extract prim
   in PP.putWord $ printf "@%s[%s](%s)" name predicate (PP.commaList pred_args)

-- ================================================================================
-- Typecheck
-- ================================================================================

typeCheckSearchPredicate ::
  (sizeT ~ SizeType ext, P.TypeCheckable sizeT) =>
  -- | name of the predicate function
  Ident ->
  -- | arguments
  [Ident] ->
  P.TypeChecker ext [P.VarType sizeT]
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

-- ================================================================================
-- Evaluation
-- ================================================================================

data SearchResult = SearchResult {element :: [P.Value SizeT], isSolution :: Bool}
  deriving (Eq, Read, Show)

type SearchResults = [SearchResult]

hasSolution :: SearchResults -> Bool
hasSolution = any isSolution

countSolutions :: SearchResults -> Int
countSolutions = length . filter isSolution

getSearchOutputs :: SearchResults -> [[P.Value SizeT]]
getSearchOutputs rets | hasSolution rets = rets & filter isSolution & map element
getSearchOutputs rets = map element rets

-- | Run the predicate on each input, and return the input along with the result.
runSearchPredicateOnAllInputs ::
  forall ext precT.
  ( P.Evaluatable ext SizeT precT
  , P.EvalReqs SizeT precT
  ) =>
  Ident ->
  [P.Value SizeT] ->
  P.Evaluator ext SearchResults
runSearchPredicateOnAllInputs predicate arg_vals = do
  pred_fun <- view $ P._funCtx . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
  let n_fixed_args = length arg_vals

  let search_tys = pred_fun ^. to P.param_types . to (drop n_fixed_args)
  let search_range = traverse P.domain search_tys

  forM search_range $ \s_vals -> do
    rets <- P.evalFun (arg_vals ++ s_vals) (P.NamedFunDef predicate pred_fun)
    return SearchResult{element = s_vals, isSolution = P.valueToBool $ head rets}

evaluatePrimCount ::
  ( P.Evaluatable ext SizeT precT
  , P.EvalReqs SizeT precT
  ) =>
  Ident ->
  [Ident] ->
  P.ProgramState SizeT ->
  P.Evaluator ext [P.Value SizeT]
evaluatePrimCount predicate args sigma = do
  arg_vals <- runReaderT ?? sigma $ forM args $ \x -> do
    view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

  res <- runSearchPredicateOnAllInputs predicate arg_vals
  return [P.FinV $ fromIntegral $ countSolutions res]

-- ================================================================================
-- Primitive: any
-- ================================================================================

-- | Basic Primitive @any@ which asserts if there is a solution.
data PrimAny sizeT precT = PrimAny {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Read, Show)

type instance SizeType (PrimAny sizeT precT) = sizeT
type instance PrecType (PrimAny sizeT precT) = precT

instance P.MapSize (PrimAny size prec) where
  type MappedSize (PrimAny size prec) size' = PrimAny size' prec
  mapSize _ PrimAny{..} = PrimAny{..}

instance IsA SearchLikePrim (PrimAny sizeT precT) where
  extract PrimAny{predicate, pred_args} = SearchLikePrim{predicate, pred_args}

parsePrimAnyWithName :: forall sizeT precT. String -> TokenParser () -> Parser (PrimAny sizeT precT)
parsePrimAnyWithName name tp = do
  [(predicate, pred_args)] <- parsePrimWithPredicates name 1 tp
  return PrimAny{predicate, pred_args}

instance P.HasFreeVars (PrimAny sizeT precT) where
  freeVarsList PrimAny{pred_args} = pred_args

instance (P.TypeCheckable sizeT) => P.TypeCheckablePrimitive (PrimAny sizeT precT) sizeT where
  typeCheckPrimitive PrimAny{predicate, pred_args} = do
    typeCheckSearchPredicate predicate pred_args
    return [P.tbool]

instance (P.EvalReqs sizeT precT) => P.Evaluatable (PrimAny sizeT precT) sizeT precT where
  eval PrimAny{predicate, pred_args} sigma = do
    arg_vals <- runReaderT ?? sigma $
      forM pred_args $ \x -> do
        view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

    res <- runSearchPredicateOnAllInputs predicate arg_vals
    return [P.toValue $ hasSolution res]

-- ================================================================================
-- Primitive: search
-- ================================================================================

-- Primitive @search@ which returns a uniformly random solution
data PrimSearch sizeT precT = PrimSearch {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Read, Show)

type instance SizeType (PrimSearch sizeT precT) = sizeT
type instance PrecType (PrimSearch sizeT precT) = precT

instance P.MapSize (PrimSearch size prec) where
  type MappedSize (PrimSearch size prec) size' = PrimSearch size' prec
  mapSize _ PrimSearch{..} = PrimSearch{..}

instance IsA SearchLikePrim (PrimSearch sizeT precT) where
  extract PrimSearch{predicate, pred_args} = SearchLikePrim{predicate, pred_args}

parsePrimSearchWithName :: String -> TokenParser () -> Parser (PrimSearch sizeT precT)
parsePrimSearchWithName name tp = do
  [(predicate, pred_args)] <- parsePrimWithPredicates name 1 tp
  return PrimSearch{predicate, pred_args}

instance P.HasFreeVars (PrimSearch sizeT precT) where
  freeVarsList PrimSearch{pred_args} = pred_args

instance (P.TypeCheckable sizeT) => P.TypeCheckablePrimitive (PrimSearch sizeT precT) sizeT where
  typeCheckPrimitive PrimSearch{predicate, pred_args} = do
    s_tys <- typeCheckSearchPredicate predicate pred_args
    return $ P.tbool : s_tys

instance (P.EvalReqs sizeT precT) => P.Evaluatable (PrimSearch sizeT precT) sizeT precT where
  eval PrimSearch{predicate, pred_args} sigma = do
    arg_vals <- runReaderT ?? sigma $ forM pred_args $ \x -> do
      view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

    res <- runSearchPredicateOnAllInputs predicate arg_vals
    let ok = P.toValue $ hasSolution res
    let outs = getSearchOutputs res
    Prob.uniform [ok : v | v <- outs]

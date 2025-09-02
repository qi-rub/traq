{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Search.Prelude (
  -- * Basic search primitives

  -- ** @any@
  PrimAny (..),
  HasPrimAny (..),
  parsePrimAnyWithName,

  -- ** @search@
  PrimSearch (..),
  HasPrimSearch (..),
  parsePrimSearchWithName,

  -- * Search-like primitives
  IsSearchLike (..),

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

import Traq.Prelude
import Traq.Primitives.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Search-like primitive: has a predicate and a list of arguments to bind to it.
-- ================================================================================
class IsSearchLike primT where
  getPredicateName :: primT -> Ident
  getPredArgs :: primT -> [Ident]

printSearchLikePrim ::
  (IsSearchLike primT, MonadWriter [String] m, MonadFail m) =>
  Ident ->
  primT ->
  m ()
printSearchLikePrim name prim =
  PP.putWord $ printf "@%s[%s](%s)" name (getPredicateName prim) (PP.commaList $ getPredArgs prim)

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
  let search_range = traverse P.domain search_tys

  forM search_range $ \s_vals -> do
    rets <- P.evalFun (arg_vals ++ s_vals) (P.NamedFunDef predicate pred_fun)
    return SearchResult{element = s_vals, isSolution = P.valueToBool $ head rets}

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

-- ================================================================================
-- Primitive: any
-- ================================================================================

-- | Basic Primitive @any@ which asserts if there is a solution.
data PrimAny = PrimAny {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Read, Show)

-- | Primitive @any@ that returns True if there is a solution to the predicate.
class HasPrimAny primT where
  -- | build a call to @any@, given the predicate and args.
  mkPrimAny :: PrimAny -> primT

  -- | Prism to access the primitive.
  _PrimAny :: Traversal' primT PrimAny

instance HasPrimAny PrimAny where
  _PrimAny = id
  mkPrimAny = id

instance IsSearchLike PrimAny where
  getPredicateName PrimAny{predicate} = predicate
  getPredArgs PrimAny{pred_args} = pred_args

parsePrimAnyWithName :: String -> TokenParser () -> Parser PrimAny
parsePrimAnyWithName name tp = do
  [(predicate, pred_args)] <- parsePrimWithPredicates name 1 tp
  return PrimAny{predicate, pred_args}

instance P.HasFreeVars PrimAny where
  freeVarsList PrimAny{pred_args} = pred_args

instance P.TypeCheckablePrimitive PrimAny where
  typeCheckPrimitive PrimAny{predicate, pred_args} = do
    typeCheckSearchPredicate predicate pred_args
    return [P.tbool]

instance
  (Fractional costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT PrimAny costT
  where
  evalPrimitive PrimAny{predicate, pred_args} sigma = do
    arg_vals <- runReaderT ?? sigma $
      forM pred_args $ \x -> do
        view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

    res <- runSearchPredicateOnAllInputs predicate arg_vals
    return [P.toValue $ hasSolution res]

-- ================================================================================
-- Primitive: search
-- ================================================================================

-- Primitive @search@ which returns a uniformly random solution
data PrimSearch = PrimSearch {predicate :: Ident, pred_args :: [Ident]}
  deriving (Eq, Read, Show)

{- | Primitive @search@ that returns a solution to the predicate if exists, otherwise returns a random value.
 Also returns a bit indicating if a solution was found.
-}
class HasPrimSearch primT where
  -- | build a call to @any@, given the predicate and args.
  mkPrimSearch :: PrimSearch -> primT

  -- | Prism to access the primitive.
  _PrimSearch :: Traversal' primT PrimSearch

instance HasPrimSearch PrimSearch where
  _PrimSearch = id
  mkPrimSearch = id

instance IsSearchLike PrimSearch where
  getPredicateName PrimSearch{predicate} = predicate
  getPredArgs PrimSearch{pred_args} = pred_args

parsePrimSearchWithName :: String -> TokenParser () -> Parser PrimSearch
parsePrimSearchWithName name tp = do
  [(predicate, pred_args)] <- parsePrimWithPredicates name 1 tp
  return PrimSearch{predicate, pred_args}

instance P.HasFreeVars PrimSearch where
  freeVarsList PrimSearch{pred_args} = pred_args

instance P.TypeCheckablePrimitive PrimSearch where
  typeCheckPrimitive PrimSearch{predicate, pred_args} = do
    s_tys <- typeCheckSearchPredicate predicate pred_args
    return $ P.tbool : s_tys

instance
  (Fractional costT, P.EvaluatablePrimitive primsT primsT costT) =>
  P.EvaluatablePrimitive primsT PrimSearch costT
  where
  evalPrimitive PrimSearch{predicate, pred_args} sigma = do
    arg_vals <- runReaderT ?? sigma $ forM pred_args $ \x -> do
      view $ P._state . Ctx.at x . non (error "invalid argument, please typecheck first.")

    res <- runSearchPredicateOnAllInputs predicate arg_vals
    let ok = P.toValue $ hasSolution res
    let outs = getSearchOutputs res
    Prob.uniform [ok : v | v <- outs]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.Prelude (
  -- * Basic search primitives
  BooleanPredicate (..),
  -- SearchLikePrim (..),

  -- ** @any@
  PrimAny (..),

  -- ** @search@
  PrimSearch (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)

import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Search-like primitive: has a predicate and a list of arguments to bind to it.
-- ================================================================================
newtype BooleanPredicate a = BooleanPredicate {predicate :: a}
  deriving (Eq, Show)

instance ValidPrimShape BooleanPredicate where
  listToShape [predicate] = Right BooleanPredicate{predicate}
  listToShape _ = Left "FindXorPeriod expects exactly one function"
  shapeToList BooleanPredicate{predicate} = [predicate]

-- ================================================================================
-- Primitive: any
-- ================================================================================

-- | Basic Primitive @any@ which asserts if there is a solution.
data PrimAny sizeT precT = PrimAny {search_ty :: P.VarType sizeT}
  deriving (Eq, Read, Show)

type instance SizeType (PrimAny sizeT precT) = sizeT
type instance PrecType (PrimAny sizeT precT) = precT

type instance PrimFnShape (PrimAny size prec) = BooleanPredicate

instance P.MapSize (PrimAny size prec) where
  type MappedSize (PrimAny size prec) size' = PrimAny size' prec
  mapSize f PrimAny{search_ty} = PrimAny{search_ty = fmap f search_ty}

instance (Show size) => SerializePrim (PrimAny size prec) where
  primNames = ["any"]
  parsePrimParams tp _ = PrimAny <$> P.varType tp
  printPrimParams PrimAny{search_ty} = [PP.toCodeWord search_ty]

instance (Eq sizeT, Num sizeT) => TypeCheckPrim (PrimAny sizeT precT) sizeT where
  inferRetTypesPrim PrimAny{search_ty} BooleanPredicate{predicate = pred_ty} = do
    when (pred_ty /= P.FnType [search_ty] [P.tbool]) $
      throwError "any: must be single-argument boolean predicate"
    return [P.tbool]

instance EvalPrim (PrimAny sizeT precT) sizeT precT where
  evalPrim PrimAny{search_ty} BooleanPredicate{predicate = eval_pred} = do
    let search_range = P.domain search_ty
    bs <- forM search_range $ \v -> do
      res <- eval_pred [v]
      case res of
        [b] -> return $ P.valueToBool b
        _ -> error "fail"

    return [P.toValue $ or bs]

-- ================================================================================
-- Primitive: search
-- ================================================================================

-- Primitive @search@ which returns a uniformly random solution
data PrimSearch sizeT precT = PrimSearch {search_ty :: P.VarType sizeT}
  deriving (Eq, Read, Show)

type instance SizeType (PrimSearch sizeT precT) = sizeT
type instance PrecType (PrimSearch sizeT precT) = precT

type instance PrimFnShape (PrimSearch size prec) = BooleanPredicate

instance P.MapSize (PrimSearch size prec) where
  type MappedSize (PrimSearch size prec) size' = PrimSearch size' prec
  mapSize f PrimSearch{search_ty} = PrimSearch{search_ty = fmap f search_ty}

instance (Show size) => SerializePrim (PrimSearch size prec) where
  primNames = ["search"]
  parsePrimParams tp _ = PrimSearch <$> P.varType tp
  printPrimParams PrimSearch{search_ty} = [PP.toCodeWord search_ty]

instance (P.TypingReqs sizeT) => TypeCheckPrim (PrimSearch sizeT precT) sizeT where
  inferRetTypesPrim PrimSearch{search_ty} BooleanPredicate{predicate = pred_ty} = do
    when (pred_ty /= P.FnType [search_ty] [P.tbool]) $
      throwError $
        "search: must be single-argument boolean predicate, got " ++ show pred_ty
    return [P.tbool, search_ty]

instance EvalPrim (PrimSearch sizeT precT) sizeT precT where
  evalPrim PrimSearch{search_ty} BooleanPredicate{predicate = eval_pred} = do
    let search_range = P.domain search_ty
    res <- forM search_range $ \v -> do
      res <- eval_pred [v]
      case res of
        [b] -> return (P.valueToBool b, v)
        _ -> error "fail"

    let ok = any fst res
    let outs = map snd $ filter ((ok ==) . fst) res
    Prob.uniform [[P.toValue ok, v] | v <- outs]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.Prelude (
  -- * Base class for Search Primitives
  PrimSearch (..),
  PrimSearchKind (..),
  BooleanPredicate (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)

import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

newtype BooleanPredicate a = BooleanPredicate {predicate :: a}
  deriving (Eq, Show)

instance ValidPrimShape BooleanPredicate where
  listToShape [predicate] = Right BooleanPredicate{predicate}
  listToShape _ = Left "FindXorPeriod expects exactly one function"
  shapeToList BooleanPredicate{predicate} = [predicate]

data PrimSearchKind
  = AnyK -- returns bool
  | AllK -- returns bool
  | SearchK -- returns bool, elem (uniform):
  deriving (Eq, Read, Show, Enum)

-- Primitive @search@ which returns a uniformly random solution
data PrimSearch sizeT precT = PrimSearch {search_kind :: PrimSearchKind, search_ty :: P.VarType sizeT}
  deriving (Eq, Read, Show)

type instance SizeType (PrimSearch sizeT precT) = sizeT
type instance PrecType (PrimSearch sizeT precT) = precT

type instance PrimFnShape (PrimSearch size prec) = BooleanPredicate

instance P.MapSize (PrimSearch size prec) where
  type MappedSize (PrimSearch size prec) size' = PrimSearch size' prec
  mapSize f PrimSearch{search_kind, search_ty} = PrimSearch{search_kind, search_ty = fmap f search_ty}

instance (Show size) => SerializePrim (PrimSearch size prec) where
  primNames = ["any", "all", "search"]
  primNameOf PrimSearch{search_kind} =
    case search_kind of
      AnyK -> "any"
      AllK -> "all"
      SearchK -> "search"

  parsePrimParams tp name = PrimSearch k <$> P.varType tp
   where
    k = case name of
      "any" -> AnyK
      "all" -> AllK
      "search" -> SearchK
      _ -> error $ "invalid search primitive `" <> name <> "`"

  printPrimParams PrimSearch{search_ty} = [PP.toCodeWord search_ty]

instance (P.TypingReqs sizeT) => TypeCheckPrim (PrimSearch sizeT precT) sizeT where
  inferRetTypesPrim PrimSearch{search_kind, search_ty} BooleanPredicate{predicate = pred_ty} = do
    when (pred_ty /= P.FnType [search_ty] [P.tbool]) $
      throwError $
        "search: must be single-argument boolean predicate, got " ++ show pred_ty
    return $ case search_kind of
      AnyK -> [P.tbool]
      AllK -> [P.tbool]
      SearchK -> [P.tbool, search_ty]

instance EvalPrim (PrimSearch sizeT precT) sizeT precT where
  evalPrim PrimSearch{search_kind, search_ty} BooleanPredicate{predicate = eval_pred} = do
    let search_range = P.domain search_ty
    res <- forM search_range $ \v -> do
      res <- eval_pred [v]
      case res of
        [b] -> return (P.valueToBool b, v)
        _ -> error "fail"

    case search_kind of
      AnyK -> return [P.toValue $ any fst res]
      AllK -> return [P.toValue $ all fst res]
      SearchK -> do
        let ok = any fst res
        let outs = map snd $ filter ((ok ==) . fst) res
        Prob.uniform [[P.toValue ok, v] | v <- outs]

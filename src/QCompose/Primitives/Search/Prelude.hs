{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives.Search.Prelude (
  IsSearch (..),
  QSearchFullImpl (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.Control.Monad (maybeWithError)
import qualified QCompose.Data.Context as Ctx
import QCompose.Prelude

import qualified QCompose.CQPL as CQPL
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

-- | Full Implementation
data QSearchFullImpl holeT sizeT costT = QSearchFullImpl
  { formulas :: P.QSearchFormulas sizeT costT
  , unitaryAlgo :: UQPL.QSearchUnitaryImpl holeT sizeT costT
  , quantumAlgo :: CQPL.QSearchCQImpl holeT sizeT costT
  }

class IsSearch primT where
  mkAny :: Ident -> primT
  mkSearch :: Ident -> primT

  getPredicate :: primT -> Ident
  returnsSol :: primT -> Bool

instance {-# OVERLAPPABLE #-} (IsSearch primT) => P.TypeCheckablePrimitive primT sizeT where
  typeCheckPrimitive prim args = do
    let predicate = getPredicate prim
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return $ P.tbool : [last param_types | returnsSol prim]

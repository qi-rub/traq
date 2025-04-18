{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives () where

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

import QCompose.Primitives.Search.QSearchCFNW

data DefaultPrims = QAny QSearchCFNW

instance P.TypeCheckablePrimitive DefaultPrims sizeT where
  typeCheckPrimitive (QAny q) = P.typeCheckPrimitive q

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.Primitives.Search.Prelude (
  HasSearch (..),
  QSearchFullImpl (..),
) where

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

class HasSearch primT where
  mkAny :: Ident -> primT
  mkSearch :: Ident -> primT

  getPredicate :: primT -> Ident
  returnsSol :: primT -> Bool

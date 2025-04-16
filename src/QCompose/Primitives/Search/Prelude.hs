module QCompose.Primitives.Search.Prelude (
  QSearchFullImpl (..),
) where

import qualified QCompose.CQPL as CQPL
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

-- | Full Implementation
data QSearchFullImpl holeT sizeT costT = QSearchFullImpl
  { formulas :: P.QSearchFormulas sizeT costT
  , unitaryAlgo :: UQPL.QSearchUnitaryImpl holeT sizeT costT
  , quantumAlgo :: CQPL.QSearchCQImpl holeT sizeT costT
  }

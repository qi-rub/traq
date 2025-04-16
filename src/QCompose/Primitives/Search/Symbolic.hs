module QCompose.Primitives.Search.Symbolic (
  qsearchSymbolic,
) where

import qualified Data.Number.Symbolic as Sym

import qualified QCompose.ProtoLang as P

import QCompose.Primitives.Search.Prelude

-- | Symbolic worst case formulas.
symbolicFormulas :: forall sizeT costT. (Show sizeT, Show costT, Integral sizeT, RealFloat costT) => P.QSearchFormulas (Sym.Sym sizeT) (Sym.Sym costT)
symbolicFormulas =
  P.QSearchFormulas
    { P.qSearchExpectedCost = error "no symbolic support for expected quantum cost"
    , P.qSearchWorstCaseCost = wcF
    , P.qSearchUnitaryCost = ucF
    }
 where
  ucF :: Sym.Sym sizeT -> Sym.Sym costT -> Sym.Sym costT
  ucF n eps = Sym.var $ "QryU(" <> show n <> ", " <> show eps <> ")"

  wcF :: Sym.Sym sizeT -> Sym.Sym costT -> Sym.Sym costT
  wcF n eps = Sym.var $ "QryQ(" <> show n <> ", " <> show eps <> ")"

qsearchSymbolic ::
  (Show sizeT, Show costT, Integral sizeT, RealFloat costT) =>
  QSearchFullImpl holeT (Sym.Sym sizeT) (Sym.Sym costT)
qsearchSymbolic =
  QSearchFullImpl
    { formulas = symbolicFormulas
    , unitaryAlgo = error "Cannot compile unitary with symbolic formulas"
    , quantumAlgo = error "Cannot compile quantum with symbolic formulas"
    }

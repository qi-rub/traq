module QCompose.Subroutines.QSearch where

import qualified QCompose.CQPL as CQ
import QCompose.Prelude
import QCompose.ProtoLang (Complexity)
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

{- | Cost formulas for quantum search from the paper
 [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
cadeEtAlFormulas :: P.QSearchFormulas
cadeEtAlFormulas =
  P.QSearchFormulas
    { P.qSearchExpectedCost = eqsearch
    , P.qSearchWorstCaseCost = eqsearch_worst
    , P.qSearchUnitaryCost = zalka
    }
 where
  eqsearch_worst :: SizeT -> FailProb -> Complexity
  eqsearch_worst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

  f :: SizeT -> SizeT -> Complexity
  f n t
    | 4 * t < n = 2.0344
    | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

  eqsearch :: SizeT -> SizeT -> FailProb -> Complexity
  eqsearch n t eps
    | t == 0 = eqsearch_worst n eps
    | otherwise = f n t * (1 + 1 / (1 - term))
   where
    term = f n t / (9.2 * sqrt (fromIntegral n))

  -- TODO verify for precision instead of fail prob
  zalka :: SizeT -> FailProb -> Complexity
  zalka n eps = 5 * fromIntegral log_fac + pi * sqrt (fromIntegral (n * log_fac))
   where
    log_fac :: SizeT
    log_fac = ceiling (log (1 / eps) / (2 * log (4 / 3)))

zalkaQSearch :: U.QSearchUnitaryImpl
zalkaQSearch =
  U.QSearchUnitaryImpl
    { U.ancillaTypes = anc
    , U.costFormulas = cadeEtAlFormulas
    }
 where
  -- anc n delta = error "TODO"
  anc _ _ = []

randomSearch :: CQ.ProcDef a
randomSearch =
  CQ.ProcDef
    { CQ.proc_name = "randomSearch"
    , CQ.proc_params = []
    , CQ.proc_body = undefined
    }

module QCompose.ProtoLang.Subroutines.QSearch (cadeEtAlFormulas) where

import QCompose.Basic
import QCompose.ProtoLang.Cost

{- | Cost formulas for quantum search from the paper
 [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
cadeEtAlFormulas :: QSearchFormulas
cadeEtAlFormulas = QSearchFormulas eqsearch eqsearch_worst zalka
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

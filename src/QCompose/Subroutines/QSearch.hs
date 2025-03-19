{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Subroutines.QSearch where

import qualified QCompose.CQPL as CQ
import QCompose.Prelude
import QCompose.ProtoLang (Complexity, VarType (..))
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

-- | Cost formulas for quantum search algorithms \( \textbf{QSearch} \) and \( \textbf{QSearch}_\text{Zalka} \).
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

-- | Ancilla and Cost formulas for the unitary quantum search algorithm \( \textbf{QSearch}_\text{Zalka} \).
zalkaQSearch :: U.QSearchUnitaryImpl
zalkaQSearch =
  U.QSearchUnitaryImpl
    { U.ancillaTypes = anc
    , U.costFormulas = cadeEtAlFormulas
    }
 where
  -- anc n delta = error "TODO"
  anc _ _ = []

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
qSearch ::
  -- | search element type
  VarType SizeT ->
  -- | N_{samples} - the number of classical samples to do first
  SizeT ->
  -- | Failure probability \eps > 0
  FailProb ->
  CQ.ProcDef SizeT
qSearch ty n_samples eps =
  CQ.ProcDef
    { CQ.proc_name = "qSearch"
    , CQ.proc_params = []
    , CQ.proc_body =
        CQ.SeqS
          [ classicalSampling
          , quantumSampling
          ]
    }
 where
  classicalSampling :: CQ.Stmt SizeT
  classicalSampling =
    CQ.ForS "k" (Fin n_samples) $
      CQ.SeqS
        [ CQ.RandomS "x" ty
        , CQ.CallS CQ.OracleCall ["x", "ok"]
        , CQ.IfThenElseS
            "ok"
            (CQ.ReturnS ["x"])
            CQ.SkipS
        ]

  Fin n = ty

  alpha = 9.2
  lambda = 6 / 5

  sqrt_n :: Float
  sqrt_n = sqrt (fromIntegral n)

  n_runs, q_max :: SizeT
  n_runs = ceiling $ logBase 3 (1 / eps)
  q_max = ceiling $ alpha * sqrt_n

  quantumSampling, quantumSamplingOneRound :: CQ.Stmt SizeT
  quantumSampling = CQ.ForS "r" (Fin n_runs) quantumSamplingOneRound
  quantumSamplingOneRound =
    CQ.SeqS
      [ CQ.AssignS ["m"] (CQ.ConstFloatE lambda)
      , CQ.AssignS ["Q_sum"] (CQ.ConstE 0)
      , CQ.RandomDynS "j" "m"
      , CQ.WhileS
          ( CQ.LEqE
              (CQ.AddE (CQ.VarE "Q_sum") (CQ.VarE "j"))
              (CQ.ConstE $ fromIntegral q_max)
          )
          ( CQ.SeqS
              [ error "TODO grover cycle"
              , CQ.CallS CQ.OracleCall ["y", "ok"]
              , CQ.IfThenElseS
                  "ok"
                  (CQ.ReturnS ["y"])
                  ( CQ.SeqS
                      [ CQ.AssignS ["Q_sum"] $ CQ.AddE (CQ.VarE "Q_sum") (CQ.AddE (CQ.VarE "j") (CQ.ConstE 1))
                      , CQ.AssignS ["m"] $ CQ.MinE (CQ.MulE (CQ.ConstFloatE lambda) (CQ.VarE "m")) (CQ.ConstFloatE sqrt_n)
                      , CQ.RandomDynS "j" "m"
                      ]
                  )
              ]
          )
      ]

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.QSearch where

import qualified Data.Number.Symbolic as Sym

import qualified QCompose.CQPL as CQ
import QCompose.Prelude
import QCompose.ProtoLang (VarType (..))
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

symbolicFormulas :: forall sizeT costT. (Show sizeT, Show costT, Integral sizeT, RealFloat costT) => P.QSearchFormulas (Sym.Sym sizeT) (Sym.Sym costT)
symbolicFormulas =
  P.QSearchFormulas
    { P.qSearchExpectedCost = error "no symbolic support"
    , P.qSearchWorstCaseCost = error "no symbolic support"
    , P.qSearchUnitaryCost = ucF
    }
 where
  ucF :: Sym.Sym sizeT -> Sym.Sym costT -> Sym.Sym costT
  ucF n eps = Sym.var $ "Qry(" <> show n <> ", " <> show eps <> ")"

-- | Cost formulas for quantum search algorithms \( \textbf{QSearch} \) and \( \textbf{QSearch}_\text{Zalka} \).
cadeEtAlFormulas :: forall sizeT costT. (Integral sizeT, RealFloat costT) => P.QSearchFormulas sizeT costT
cadeEtAlFormulas =
  P.QSearchFormulas
    { P.qSearchExpectedCost = eqsearch
    , P.qSearchWorstCaseCost = eqsearch_worst
    , P.qSearchUnitaryCost = zalka
    }
 where
  eqsearch_worst :: sizeT -> costT -> costT
  eqsearch_worst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

  f :: sizeT -> sizeT -> costT
  f n t
    | 4 * t < n = 2.0344
    | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

  eqsearch :: sizeT -> sizeT -> costT -> costT
  eqsearch n t eps
    | t == 0 = eqsearch_worst n eps
    | otherwise = f n t * (1 + 1 / (1 - term))
   where
    term = f n t / (9.2 * sqrt (fromIntegral n))

  -- TODO verify for precision instead of fail prob
  zalka :: sizeT -> costT -> costT
  zalka n eps = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)
   where
    -- log_fac = ceiling log_fac
    log_fac :: costT
    log_fac = log (1 / eps) / (2 * log (4 / 3))

-- | Ancilla and Cost formulas for the unitary quantum search algorithm \( \textbf{QSearch}_\text{Zalka} \).
zalkaQSearch :: (RealFloat costT) => U.QSearchUnitaryImpl costT
zalkaQSearch =
  U.QSearchUnitaryImpl
    { U.ancillaTypes = anc
    , U.costFormulas = cadeEtAlFormulas
    }
 where
  -- anc n delta = error "TODO"
  anc _ _ = []

-- Primitives to implement Grover search and its variants
groverIteration ::
  (RealFloat costT) =>
  -- | registers to search (reflect) over
  [Ident] ->
  -- | flag register (output of predicate)
  Ident ->
  -- | phase oracle
  U.Stmt a costT ->
  U.Stmt a costT
groverIteration xs flag ph_oracle =
  U.SeqS
    [ ph_oracle
    , U.UnitaryS xs $ U.Unif undefined
    , U.UnitaryS (xs ++ [flag]) $ U.BlackBoxU $ U.BlackBox "MCX"
    , U.UnitaryS xs $ U.UnifDagger undefined
    ]

zalkaQSearchImpl ::
  (RealFloat costT) =>
  U.ProcDef a costT ->
  U.Stmt a costT
zalkaQSearchImpl = undefined

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
    , CQ.proc_local_vars = []
    , CQ.proc_body =
        CQ.SeqS
          [ classicalSampling
          , quantumSampling
          ]
    }
 where
  classicalSampling :: CQ.Stmt
  classicalSampling =
    CQ.ForS "k" (CQ.ConstE $ CQ.IntV n_samples) $
      CQ.SeqS
        [ CQ.RandomS "x" (CQ.ConstE (CQ.IntV n))
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

  quantumSampling, quantumSamplingOneRound :: CQ.Stmt
  quantumSampling = CQ.ForS "r" (CQ.ConstE $ CQ.IntV n_runs) quantumSamplingOneRound
  quantumSamplingOneRound =
    CQ.SeqS
      [ CQ.AssignS ["m"] (CQ.ConstE (CQ.FloatV lambda))
      , CQ.AssignS ["Q_sum"] (CQ.ConstE (CQ.IntV 0))
      , CQ.RandomS "j" (CQ.VarE "m")
      , CQ.WhileS
          ( CQ.LEqE
              (CQ.AddE (CQ.VarE "Q_sum") (CQ.VarE "j"))
              (CQ.ConstE $ CQ.IntV q_max)
          )
          ( CQ.SeqS
              [ error "TODO grover cycle"
              , CQ.CallS CQ.OracleCall ["y", "ok"]
              , CQ.IfThenElseS
                  "ok"
                  (CQ.ReturnS ["y"])
                  ( CQ.SeqS
                      [ CQ.AssignS ["Q_sum"] $ CQ.AddE (CQ.VarE "Q_sum") (CQ.AddE (CQ.VarE "j") (CQ.ConstE $ CQ.IntV 1))
                      , CQ.AssignS ["m"] $ CQ.MinE (CQ.MulE (CQ.ConstE (CQ.FloatV lambda)) (CQ.VarE "m")) (CQ.ConstE (CQ.FloatV sqrt_n))
                      , CQ.RandomS "j" (CQ.VarE "m")
                      ]
                  )
              ]
          )
      ]

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.QSearch (
  symbolicFormulas,
  cadeEtAlFormulas,
  zalkaQSearch,
) where

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

  zalka :: sizeT -> costT -> costT
  zalka n delta = 2 * nq -- for compute-uncompute
   where
    -- fail prob
    eps = (delta / 2) ^ (2 :: Int)

    -- log_fac = ceiling log_fac
    log_fac :: costT
    log_fac = log (1 / eps) / (2 * log (4 / 3))

    nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

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
  U.Stmt sizeT costT ->
  U.Stmt sizeT costT
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

-- | the while loop
whileK ::
  -- | iteration limit
  sizeT ->
  -- | loop condition
  Ident ->
  -- | loop body
  CQ.Stmt sizeT ->
  CQ.Stmt sizeT
whileK k cond body = CQ.RepeatS k $ CQ.IfThenElseS cond body CQ.SkipS

whileKWithExpr ::
  -- | iteration limit
  sizeT ->
  -- | loop condition variable
  Ident ->
  -- | loop condition expression
  CQ.Expr sizeT ->
  -- | loop body
  CQ.Stmt sizeT ->
  CQ.Stmt sizeT
whileKWithExpr k cond_var cond_expr body =
  CQ.SeqS
    [ compute_cond
    , whileK k cond_var (CQ.SeqS [body, compute_cond])
    ]
 where
  compute_cond = CQ.AssignS [cond_var] cond_expr

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
qSearch ::
  (RealFloat costT) =>
  -- | search element type
  VarType SizeT ->
  -- | N_{samples} - the number of classical samples to do first
  SizeT ->
  -- | Failure probability \eps > 0
  costT ->
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
  classicalSampling :: CQ.Stmt SizeT
  classicalSampling =
    whileKWithExpr n_samples "not_done" (CQ.NotE $ CQ.VarE "ok") $
      CQ.SeqS
        [ CQ.RandomS "x" (Fin n)
        , CQ.CallS CQ.OracleCall ["x", "ok"]
        ]

  Fin n = ty

  alpha = 9.2
  lambda = 6 / 5

  sqrt_n :: Float
  sqrt_n = sqrt (fromIntegral n)

  n_runs, q_max :: SizeT
  n_runs = ceiling $ logBase 3 (1 / eps)
  q_max = ceiling $ alpha * sqrt_n

  -- compute the limits for sampling `j` in each iteration.
  sampling_ranges :: [SizeT]
  sampling_ranges = go q_max js
   where
    go :: SizeT -> [SizeT] -> [SizeT]
    go _ [] = []
    go lim (x : _) | x > lim = []
    go lim (x : xs) = x : go (lim - x) xs

    js :: [SizeT]
    js = map floor js_f

    js_f :: [Float]
    js_f = lambda : map nxt js_f

    nxt :: Float -> Float
    nxt m = min (lambda * m) sqrt_n

  quantumSampling, quantumSamplingOneRound :: CQ.Stmt SizeT
  quantumSampling = CQ.RepeatS n_runs quantumSamplingOneRound
  quantumSamplingOneRound =
    CQ.SeqS
      [ CQ.IfThenElseS "ok" CQ.SkipS (quantumGroverOnce j)
      | j <- sampling_ranges
      ]

  quantumGroverOnce j =
    CQ.SeqS
      [ error $ "TODO call and meas: grover cycle" <> show j
      , CQ.CallS CQ.OracleCall ["y", "ok"]
      ]

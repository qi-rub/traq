{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.QSearch (
  symbolicFormulas,
  cadeEtAlFormulas,
  zalkaQSearch,
  groverK,
  zalkaQSearchImpl,
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

-- | Run K grover iterations
groverK ::
  forall sizeT costT.
  (RealFloat costT) =>
  -- | number of rounds
  sizeT ->
  -- | the element and type to search for. @x : T@
  (Ident, VarType sizeT) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> U.Stmt sizeT costT) ->
  U.Stmt sizeT costT
groverK k (x, ty) b mk_pred = U.SeqS [prep, U.RepeatS k grover_iterate]
 where
  -- map b to |-> and x to uniform
  prep :: U.Stmt sizeT costT
  prep =
    U.SeqS
      [ U.UnitaryS [b] U.XGate
      , U.UnitaryS [b] U.HGate
      , U.UnitaryS [x] (U.Unif ty)
      ]

  grover_iterate :: U.Stmt sizeT costT
  grover_iterate =
    U.SeqS
      [ mk_pred x b
      , U.UnitaryS [x] (U.UnifDagger ty)
      , U.UnitaryS [x] (U.Refl0 ty)
      , U.UnitaryS [x] (U.Unif ty)
      ]

{- | Grover search with fail prob 0
 when the number of solutions is known
-}
groverCertainty ::
  forall sizeT costT.
  (RealFloat costT) =>
  -- | the element and type to search for. @x : T@
  (Ident, VarType sizeT) ->
  -- | number of solutions
  sizeT ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> U.Stmt sizeT costT) ->
  U.Stmt sizeT costT
groverCertainty (x, Fin n) m b mk_pred = error "TODO"

zalkaQSearchImpl ::
  forall sizeT costT.
  (Integral sizeT, RealFloat costT) =>
  -- | type of the element to search over
  VarType sizeT ->
  -- | call the predicate on @x, b@
  (Ident -> Ident -> U.Stmt sizeT costT) ->
  -- | max. failure probability @\eps@
  costT ->
  U.Stmt sizeT costT
zalkaQSearchImpl ty mk_pred eps = error "TODO"
 where
  t0 :: sizeT
  t0 = ceiling $ logBase (4 / 3) (1 / eps) / 2

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
    CQ.whileKWithCondExpr n_samples "not_done" (CQ.NotE $ CQ.VarE "ok") $
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

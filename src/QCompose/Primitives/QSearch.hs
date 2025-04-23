{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.QSearch (
  -- * A full implementation of quantum search.
  qsearchCFNW,
) where

import Data.Maybe (catMaybes)

import qualified QCompose.CQPL as CQ
import QCompose.Prelude
import QCompose.ProtoLang (VarType (..))
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

import QCompose.Primitives.Search.Prelude

-- | Temporary black-boxes for un-implemented parts of the quantum search algorithms.
data QSearchBlackBoxes costT
  = QSearchBlackBox {q_pred_name :: Ident, n_pred_calls :: costT}
  | TODOHole String
  deriving (Eq, Show)

instance UQPL.HoleCost (QSearchBlackBoxes costT) costT where
  holeCost QSearchBlackBox{q_pred_name, n_pred_calls} = do
    pred_cost <- UQPL.procCost q_pred_name
    return $ n_pred_calls * pred_cost
  holeCost (TODOHole s) = error $ "no cost of unknown hole: " <> s

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
zalkaQSearch :: (RealFloat costT) => UQPL.QSearchUnitaryImpl (QSearchBlackBoxes costT) SizeT costT
zalkaQSearch =
  UQPL.QSearchUnitaryImpl
    { UQPL.ancillaTypes = anc
    , UQPL.costFormulas = cadeEtAlFormulas
    , UQPL.algorithm = zalkaQSearchImpl
    }
 where
  -- anc n delta = error "TODO"
  anc _ _ = []

-- | Run K grover iterations
groverK ::
  forall holeT sizeT.
  -- | number of rounds
  sizeT ->
  -- | the element and type to search for. @x : T@
  (Ident, VarType sizeT) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  UQPL.Stmt holeT sizeT
groverK k (x, ty) b mk_pred = UQPL.SeqS [prep, UQPL.RepeatS k grover_iterate]
 where
  -- map b to |-> and x to uniform
  prep :: UQPL.Stmt holeT sizeT
  prep =
    UQPL.SeqS
      [ UQPL.UnitaryS [b] UQPL.XGate
      , UQPL.UnitaryS [b] UQPL.HGate
      , UQPL.UnitaryS [x] (UQPL.Unif ty)
      ]

  grover_iterate :: UQPL.Stmt holeT sizeT
  grover_iterate =
    UQPL.SeqS
      [ mk_pred x b
      , UQPL.UnitaryS [x] (UQPL.UnifDagger ty)
      , UQPL.UnitaryS [x] (UQPL.Refl0 ty)
      , UQPL.UnitaryS [x] (UQPL.Unif ty)
      ]

{- | Grover search with fail prob 0
 when the number of solutions is known
-}
groverCertainty ::
  forall holeT sizeT.
  -- | the element and type to search for. @x : T@
  (Ident, VarType sizeT) ->
  -- | number of solutions
  sizeT ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  UQPL.Stmt holeT sizeT
groverCertainty (x, Fin n) m b mk_pred = error "TODO GroverCertainty circuit"

zalkaQSearchImpl ::
  forall sizeT costT.
  (Integral sizeT, RealFloat costT) =>
  -- | type of the element to search over
  VarType sizeT ->
  -- | call the predicate on @x, b@
  (Ident -> Ident -> UQPL.Stmt (QSearchBlackBoxes costT) sizeT) ->
  -- | max. failure probability @\eps@
  costT ->
  UQPL.Stmt (QSearchBlackBoxes costT) sizeT
zalkaQSearchImpl ty mk_pred eps =
  UQPL.HoleS
    { UQPL.hole =
        QSearchBlackBox
          { q_pred_name
          , n_pred_calls = P.qSearchUnitaryCost cadeEtAlFormulas n eps
          }
    , UQPL.dagger = False
    }
 where
  q_pred_name = case mk_pred "pred_arg" "pred_res" of
    UQPL.CallS{UQPL.proc_id} -> proc_id
    _ -> error "predicate is not a call!"
  Fin n = ty

  t0 :: sizeT
  t0 = ceiling $ logBase (4 / 3) (1 / eps) / 2

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
qSearch ::
  forall costT.
  (RealFloat costT) =>
  CQ.QSearchAlgorithm (QSearchBlackBoxes costT) SizeT costT
qSearch ty n_samples eps upred_caller pred_caller params =
  CQ.ProcDef
    { CQ.proc_name = "qSearch"
    , CQ.proc_meta_params = []
    , CQ.proc_param_types = map snd params
    , CQ.mproc_body =
        Just
          CQ.ProcBody
            { CQ.proc_param_names = map fst params
            , CQ.proc_local_vars =
                [ ("not_done", P.Fin 2)
                , ("Q_sum", j_type)
                , ("j", j_type)
                ]
            , CQ.proc_body_stmt =
                CQ.SeqS $
                  catMaybes
                    [ if n_samples /= 0 then Just classicalSampling else Nothing
                    , Just quantumSampling
                    ]
            }
    , CQ.is_oracle = False
    }
 where
  classicalSampling :: CQ.Stmt (QSearchBlackBoxes costT) SizeT
  classicalSampling =
    CQ.WhileKWithCondExpr n_samples "not_done" (CQ.NotE $ CQ.VarE "ok") $
      CQ.SeqS
        [ CQ.RandomS "x" (Fin n)
        , pred_caller "x" "ok"
        ]

  Fin n = ty

  alpha = 9.2
  lambda = 6 / 5

  sqrt_n :: Float
  sqrt_n = sqrt (fromIntegral n)

  n_runs, q_max :: SizeT
  n_runs = ceiling $ logBase 3 (1 / eps)
  q_max = ceiling $ alpha * sqrt_n

  -- type for j and Q_sum
  j_type = P.Fin q_max

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

  quantumSampling, quantumSamplingOneRound :: CQ.Stmt (QSearchBlackBoxes costT) SizeT
  quantumSampling = CQ.RepeatS n_runs quantumSamplingOneRound
  quantumSamplingOneRound =
    CQ.SeqS $
      CQ.AssignS ["Q_sum"] (CQ.ConstE{CQ.val = 0, CQ.val_ty = j_type})
        : map quantumGroverOnce sampling_ranges

  -- one call and meas to grover with j iterations
  quantumGroverOnce j_lim =
    CQ.SeqS
      [ CQ.RandomS "j" (P.Fin $ j_lim + 1)
      , CQ.AssignS ["Q_sum"] (CQ.AddE (CQ.VarE "Q_sum") (CQ.VarE "j"))
      , CQ.AssignS ["not_done"] (CQ.LEqE (CQ.VarE "Q_sum") (CQ.ConstE{CQ.val = fromIntegral j_lim, CQ.val_ty = j_type}))
      , CQ.IfThenElseS "not_done" (CQ.HoleS "callandmeas: grover cycle j") CQ.SkipS
      , pred_caller "y" "ok"
      ]

qSearchCQImpl :: (RealFloat costT) => CQ.QSearchCQImpl (QSearchBlackBoxes costT) SizeT costT
qSearchCQImpl =
  CQ.QSearchCQImpl
    { CQ.costFormulas = cadeEtAlFormulas
    , CQ.qsearchAlgo = qSearch
    , CQ.unitaryImpl = zalkaQSearch
    }

-- | Search Algorithms from https://arxiv.org/abs/2203.04975
qsearchCFNW :: (RealFloat costT) => QSearchFullImpl (QSearchBlackBoxes costT) SizeT costT
qsearchCFNW =
  QSearchFullImpl
    { formulas = cadeEtAlFormulas
    , unitaryAlgo = zalkaQSearch
    , quantumAlgo = qSearchCQImpl
    }

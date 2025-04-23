{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.QSearch (
  -- * TODO Remove
  zalkaQSearchImpl,
) where

import Data.Maybe (catMaybes)

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import QCompose.Primitives.Search.QSearchCFNW (_QSearchZalka)
import QCompose.ProtoLang (VarType (..))
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

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

-- | Shape of a QSearch implementation
type QSearchAlgorithm holeT sizeT costT =
  -- | search elem type
  P.VarType sizeT ->
  -- | number of classical samples
  sizeT ->
  -- | max fail prob
  costT ->
  -- | unitary predicate caller
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  -- | cqpl predicate caller
  (Ident -> Ident -> CQPL.Stmt holeT sizeT) ->
  -- | arguments to the function
  [(Ident, P.VarType sizeT)] ->
  -- | the generated QSearch procedure
  CQPL.ProcDef holeT sizeT

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
          , n_pred_calls = _QSearchZalka n eps
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
  QSearchAlgorithm (QSearchBlackBoxes costT) SizeT costT
qSearch ty n_samples eps upred_caller pred_caller params =
  CQPL.ProcDef
    { CQPL.proc_name = "qSearch"
    , CQPL.proc_meta_params = []
    , CQPL.proc_param_types = map snd params
    , CQPL.mproc_body =
        Just
          CQPL.ProcBody
            { CQPL.proc_param_names = map fst params
            , CQPL.proc_local_vars =
                [ ("not_done", P.Fin 2)
                , ("Q_sum", j_type)
                , ("j", j_type)
                ]
            , CQPL.proc_body_stmt =
                CQPL.SeqS $
                  catMaybes
                    [ if n_samples /= 0 then Just classicalSampling else Nothing
                    , Just quantumSampling
                    ]
            }
    , CQPL.is_oracle = False
    }
 where
  classicalSampling :: CQPL.Stmt (QSearchBlackBoxes costT) SizeT
  classicalSampling =
    CQPL.WhileKWithCondExpr n_samples "not_done" (CQPL.NotE $ CQPL.VarE "ok") $
      CQPL.SeqS
        [ CQPL.RandomS "x" (Fin n)
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

  quantumSampling, quantumSamplingOneRound :: CQPL.Stmt (QSearchBlackBoxes costT) SizeT
  quantumSampling = CQPL.RepeatS n_runs quantumSamplingOneRound
  quantumSamplingOneRound =
    CQPL.SeqS $
      CQPL.AssignS ["Q_sum"] (CQPL.ConstE{CQPL.val = 0, CQPL.val_ty = j_type})
        : map quantumGroverOnce sampling_ranges

  -- one call and meas to grover with j iterations
  quantumGroverOnce j_lim =
    CQPL.SeqS
      [ CQPL.RandomS "j" (P.Fin $ j_lim + 1)
      , CQPL.AssignS ["Q_sum"] (CQPL.AddE (CQPL.VarE "Q_sum") (CQPL.VarE "j"))
      , CQPL.AssignS ["not_done"] (CQPL.LEqE (CQPL.VarE "Q_sum") (CQPL.ConstE{CQPL.val = fromIntegral j_lim, CQPL.val_ty = j_type}))
      , CQPL.IfThenElseS "not_done" (CQPL.HoleS $ TODOHole "callandmeas: grover cycle j") CQPL.SkipS
      , pred_caller "y" "ok"
      ]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module Traq.Primitives.Search.QSearchCFNW (
  -- * Search Primitive
  QSearchCFNW (..),

  -- * Unitary Implementation
  UQSearchEnv (..),
  algoQSearchZalka,

  -- * CQ Implementation
  groverK,

  -- * Cost Formulas
  _EQSearch,
  _EQSearchWorst,
  _QSearchZalka,
) where

import Control.Monad (replicateM, when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (censor, listen)
import Data.Maybe (fromJust)
import Data.String (fromString)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import Traq.CPL (notE, (.&&.), (.+.), (.<=.))
import qualified Traq.CPL as CPL
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- --------------------------------------------------------------------------------
-- QSearch: from https://arxiv.org/pdf/2203.04975v2
-- --------------------------------------------------------------------------------

_QSearch_alpha :: (Floating prec) => prec
_QSearch_alpha = 9.2

-- | https://arxiv.org/pdf/2203.04975v2, Eq. 4
_EQSearchWorst :: forall size prec. (Integral size, Floating prec) => size -> A.FailProb prec -> prec
_EQSearchWorst n eps = (max_iter_per_rep + 1) * n_reps
 where
  -- using +1 instead of ceiling
  max_iter_per_rep = _QSearch_alpha * sqrt (fromIntegral n) + 1
  n_reps = logBase 3 (1 / A.getFailProb eps) + 1

-- | https://arxiv.org/pdf/2203.04975v2, Eq. 3
_F :: forall size prec. (Integral size, Floating prec) => size -> size -> prec
_F n t
  | 4 * t >= n = 2.0344
  | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

-- | https://arxiv.org/pdf/2203.04975v2, Eq. 2
_EQSearch :: forall size prec. (Integral size, Floating prec) => size -> size -> A.FailProb prec -> prec
_EQSearch n t eps
  | t == 0 = _EQSearchWorst n eps
  | otherwise = _F n t * (1 + 1 / (1 - term))
 where
  term = _F n t / (_QSearch_alpha * sqrt (fromIntegral n))

-- --------------------------------------------------------------------------------
-- QSearch_Zalka: from https://arxiv.org/abs/quant-ph/9711070
-- --------------------------------------------------------------------------------

_QSearchZalka :: forall size prec. (Integral size, Floating prec) => size -> A.FailProb prec -> prec
_QSearchZalka n eps = nq_simple
 where
  -- Section 2.1 simple algorithm cost
  -- +1 for extra query at the end to obtain the flag bit (and result)
  nq_simple :: prec
  nq_simple = fromIntegral (_QSearchZalka_max_iter n + 1) * _QSearchZalka_n_reps eps

-- Section 2.2 Improved Algorithm (i.e. sqrt log(1/eps) instead of log(1/eps))
-- log_fac = ceiling log_fac
-- log_fac :: prec
-- log_fac = log (1 / eps) / (2 * log (4 / 3))

-- number of queries of the original algorithm.
-- nq :: prec
-- nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

-- Section 2.1 simple algorithm cost
_QSearchZalka_max_iter :: forall size. (Integral size) => size -> size
_QSearchZalka_max_iter n = ceiling $ (pi / 4) * sqrt (fromIntegral n :: Double)

-- Section 2.1 simple algorithm cost
_QSearchZalka_n_reps :: forall prec. (Floating prec) => A.FailProb prec -> prec
_QSearchZalka_n_reps eps = 1 + logBase (1 - p) (A.getFailProb eps)
 where
  p = 0.3914 :: prec

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

newtype QSearchCFNW size prec = QSearchCFNW (PrimSearch size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (QSearchCFNW size prec) = size
type instance PrecType (QSearchCFNW size prec) = prec

type instance PrimFnShape (QSearchCFNW size prec) = BooleanPredicate

instance CPL.MapSize (QSearchCFNW size prec) where
  type MappedSize (QSearchCFNW size prec) size' = QSearchCFNW size' prec

  mapSize f (QSearchCFNW p) = QSearchCFNW (CPL.mapSize f p)

instance PrimSearch size prec :<: QSearchCFNW size prec where
  inject = QSearchCFNW
  project (QSearchCFNW p) = Just p

instance (Show size) => SerializePrim (QSearchCFNW size prec) where
  primNames = ["any", "search"]
  primNameOf (QSearchCFNW p) = primNameOf p
  parsePrimParams tp s = QSearchCFNW <$> parsePrimParams tp s
  printPrimParams (QSearchCFNW prim) = printPrimParams prim

-- Type check
instance (CPL.TypingReqs size) => TypeCheckPrim (QSearchCFNW size prec) size where
  inferRetTypesPrim (QSearchCFNW prim) = inferRetTypesPrim prim

-- Eval
instance EvalPrim (QSearchCFNW size prec) size prec where
  evalPrim (QSearchCFNW prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

getSearchType :: QSearchCFNW size prec -> CPL.VarType size
getSearchType (QSearchCFNW (PrimSearch{search_ty})) = search_ty

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  (CPL.TypingReqs size, Integral size, Floating prec) =>
  UnitaryCostPrim (QSearchCFNW size prec) size prec
  where
  unitaryQueryCosts prim eps = BooleanPredicate $ strongQueries $ _QSearchZalka _N eps
   where
    _N = CPL.domainSize $ getSearchType prim

  unitaryExprCosts _ _ = Alg.zero

instance
  (CPL.TypingReqs size, Integral size, A.SizeToPrec size prec, Floating prec) =>
  QuantumHavocCostPrim (QSearchCFNW size prec) size prec
  where
  quantumQueryCostsUnitary prim eps = BooleanPredicate $ strongQueries $ _EQSearchWorst _N eps
   where
    _N = CPL.domainSize $ getSearchType prim

  quantumQueryCostsQuantum _ _ = BooleanPredicate 0

  quantumExprCosts = Alg.zero

instance (size ~ SizeT, Floating prec, Prob.RVType prec prec) => QuantumExpCostPrim (QSearchCFNW size prec) size prec where
  quantumExpQueryCostsUnitary prim eps (BooleanPredicate eval_pred) =
    BooleanPredicate $ strongQueries $ _EQSearch _N _K eps
   where
    search_ty = getSearchType prim
    _N = CPL.domainSize search_ty

    flags =
      CPL.domain search_ty <&> \v -> do
        [b] <- Prob.toDeterministicValue $ eval_pred [v]
        pure $ CPL.valueToBool b

    _K = length $ filter fromJust flags

  quantumExpQueryCostsQuantum _ _ _ = BooleanPredicate []

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Unitary Lowering
-- ================================================================================

-- | Information for building QSearch_Zalka
data UQSearchEnv size = UQSearchEnv
  { search_arg_type :: CPL.VarType size
  , pred_call_builder :: CQPL.Arg size -> CQPL.Arg size -> CQPL.Arg size -> CQPL.UStmt size
  }

-- | A layer on top of the unitary compiler, holding the relevant QSearch context, and storing the produced statements.
type UQSearchBuilder ext =
  RWST
    (UQSearchEnv (SizeType ext))
    [CQPL.UStmt (SizeType ext)]
    ()
    (Compiler.CompilerT ext)

addPredCall :: (size ~ SizeType ext) => CQPL.Arg size -> CQPL.Arg size -> CQPL.Arg size -> UQSearchBuilder ext ()
addPredCall c x b = do
  mk_pred <- view $ to pred_call_builder
  writeElem $ mk_pred c x b

withComputed :: (size ~ SizeType ext) => CQPL.UStmt size -> UQSearchBuilder ext a -> UQSearchBuilder ext a
withComputed s m = do
  writeElem s
  a <- m
  writeElem $ CQPL.adjoint s
  return a

addGroverIteration ::
  forall ext size prec.
  ( Integral size
  , RealFloat prec
  , CPL.TypingReqs size
  , size ~ SizeType ext
  , prec ~ PrecType ext
  ) =>
  -- | ctrl
  CQPL.Arg size ->
  -- | x
  CQPL.Arg size ->
  -- | b
  CQPL.Arg size ->
  UQSearchBuilder ext ()
addGroverIteration c x b = do
  x_ty <- view $ to search_arg_type
  let unifX = CQPL.DistrU (CPL.UniformE x_ty)
  addPredCall c x b
  writeElem $ CQPL.UnitaryS [x] (CQPL.Adjoint unifX)
  writeElem $ CQPL.UnitaryS [x] (CQPL.BasicGateU (CQPL.PhaseOnZero pi)) -- reflect on |0>
  writeElem $ CQPL.UnitaryS [x] unifX

algoQSearchZalkaRandomIterStep ::
  forall ext size prec.
  ( Integral size
  , RealFloat prec
  , CPL.TypingReqs size
  , size ~ SizeType ext
  , prec ~ PrecType ext
  ) =>
  -- | max num of iteration
  size ->
  CQPL.Arg size ->
  CQPL.Arg size ->
  CQPL.Arg size ->
  CQPL.Arg size ->
  UQSearchBuilder ext ()
algoQSearchZalkaRandomIterStep r r_reg ctrl_bit x_reg b_reg = do
  let r_ty = CPL.Fin r
  x_ty <- view $ to search_arg_type

  -- uniform r
  let prep_r = CQPL.UnitaryS [r_reg] (CQPL.DistrU (CPL.UniformE r_ty))

  withComputed prep_r $ do
    -- b in minus state for grover
    let prep_b =
          CQPL.USeqS
            [ CQPL.UnitaryS [b_reg] (CQPL.BasicGateU CQPL.XGate)
            , CQPL.UnitaryS [b_reg] (CQPL.BasicGateU CQPL.HGate)
            ]
    withComputed prep_b $ do
      -- uniform x
      writeElem $ CQPL.UnitaryS [x_reg] (CQPL.DistrU (CPL.UniformE x_ty))

      -- controlled iterate
      let meta_ix_name = "LIM"
      let calc_ctrl =
            CQPL.UnitaryS [r_reg, ctrl_bit] $ CQPL.RevEmbedU ["a"] $ "a" .<=. CPL.ParamE meta_ix_name
      ((), grover_body) <-
        censor (const mempty) $
          listen $
            withComputed calc_ctrl $ do
              addGroverIteration ctrl_bit x_reg b_reg
      writeElem $ CQPL.mkForInRangeS meta_ix_name (CPL.MetaSize r) (CQPL.USeqS grover_body)

  withComputed (CQPL.UnitaryS [ctrl_bit] (CQPL.BasicGateU CQPL.XGate)) $
    addPredCall ctrl_bit x_reg b_reg

algoQSearchZalka ::
  forall ext size prec.
  ( Integral size
  , RealFloat prec
  , CPL.TypingReqs size
  , size ~ SizeType ext
  , prec ~ PrecType ext
  ) =>
  -- | max. error (TV/trace-norm)
  A.FailProb prec ->
  -- | output bit
  Ident ->
  -- | output value of search
  Ident ->
  UQSearchBuilder ext ()
algoQSearchZalka eps out_bit out_val = do
  s_ty <- view $ to search_arg_type
  let n = CPL.domainSize s_ty

  let n_iter = floor (_QSearchZalka_n_reps eps) :: size

  ctrl_bits <- lift $ Compiler.allocAncillaWithPref "ctrl" (CPL.Arr n_iter CPL.tbool)
  b_regs <- lift $ Compiler.allocAncillaWithPref "pred_out" (CPL.Arr n_iter CPL.tbool)

  let r = _QSearchZalka_max_iter n
  let r_ty = CPL.Fin r
  r_regs <- lift $ Compiler.allocAncillaWithPref "n_iter" (CPL.Arr n_iter r_ty)

  x_regs <- lift $ Compiler.allocAncillaWithPref "s_arg" (CPL.Arr n_iter s_ty)

  let iter_meta_var = "run_ix"
  censor
    ( \ss ->
        [ CQPL.UForInRangeS
            { iter_meta_var
            , iter_lim = CPL.MetaSize n_iter
            , uloop_body = CQPL.USeqS ss
            , dagger = False
            }
        ]
    )
    $ do
      algoQSearchZalkaRandomIterStep
        r
        (CQPL.ArrElemArg (CQPL.Arg r_regs) (CPL.MetaName iter_meta_var))
        (CQPL.ArrElemArg (CQPL.Arg ctrl_bits) (CPL.MetaName iter_meta_var))
        (CQPL.ArrElemArg (CQPL.Arg x_regs) (CPL.MetaName iter_meta_var))
        (CQPL.ArrElemArg (CQPL.Arg b_regs) (CPL.MetaName iter_meta_var))

  writeElem $
    CQPL.UnitaryS
      { CQPL.qargs = [CQPL.Arg b_regs, CQPL.Arg out_bit]
      , CQPL.unitary = CQPL.RevEmbedU ["a"] $ CPL.UnOpE CPL.AnyOp "a"
      }

  writeElem $
    CQPL.UnitaryS
      { CQPL.qargs = [CQPL.Arg x_regs, CQPL.Arg b_regs, CQPL.Arg out_val]
      , CQPL.unitary = CQPL.RevEmbedU ["a", "f"] $ CPL.BinOpE CPL.VecSelectOp "a" "f"
      }

instance
  (CPL.TypingReqs size, Integral size, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (QSearchCFNW size prec) size prec
  where
  compileUPrim (QSearchCFNW PrimSearch{search_kind, search_ty}) eps = do
    -- info to call the predicate
    (BooleanPredicate call_pred) <- view $ to mk_ucall
    (BooleanPredicate pred_aux_tys) <- view $ to uproc_aux_types

    ret_tys <- view $ to prim_ret_types
    (ret, x_out) <- case search_kind of
      SearchK -> do
        when (length ret_tys /= 2) $ throwError "search must return (bool, T)"
        b <- Compiler.newIdent "ret"
        x_out <- Compiler.newIdent "ret"
        pure (b, x_out)
      _ -> do
        when (length ret_tys /= 1) $ throwError "bool predicate must return single bool"
        b <- Compiler.newIdent "ret"
        x_out <- lift $ Compiler.allocAncillaWithPref "s_result" search_ty
        return (b, x_out)

    -- function to call the predicate, re-using the same aux space each time.
    pred_ancilla <- lift $ mapM Compiler.allocAncilla pred_aux_tys
    b' <- lift $ Compiler.allocAncilla CPL.tbool
    let pred_caller ctrl x b =
          let pred_call_s = call_pred (x : CQPL.Arg b' : map CQPL.Arg pred_ancilla)
              pred_call_s_full = case search_kind of
                AllK -> CQPL.USeqS [pred_call_s, CQPL.UnitaryS [CQPL.Arg b'] (CQPL.BasicGateU CQPL.XGate)]
                _ -> pred_call_s
           in CQPL.USeqS
                [ pred_call_s_full
                , CQPL.UnitaryS
                    { CQPL.qargs = [ctrl, CQPL.Arg b', b]
                    , CQPL.unitary = CQPL.BasicGateU CQPL.Toffoli
                    }
                , CQPL.adjoint pred_call_s_full
                ]

    -- Emit the qsearch procedure
    -- body:
    (qsearch_body, qsearch_ancilla) <- lift $ do
      ini_binds <- use CPL._typingCtx
      ((), ss) <- (\m -> evalRWST m UQSearchEnv{search_arg_type = search_ty, pred_call_builder = pred_caller} ()) $ algoQSearchZalka eps ret x_out
      fin_binds <- use CPL._typingCtx
      let ancillas = Ctx.toList $ fin_binds Ctx.\\ ini_binds

      let body = case search_kind of
            AllK -> CQPL.USeqS $ ss ++ [CQPL.UnitaryS [CQPL.Arg ret] (CQPL.BasicGateU CQPL.XGate)]
            _ -> CQPL.USeqS ss

      return (body, (b', CPL.tbool) : ancillas)

    -- name:
    let prim_name = (case search_kind of AnyK -> "UAny"; AllK -> "UAll"; SearchK -> "USearch")
    qsearch_proc_name <- lift $ Compiler.newIdent prim_name
    let info_comment =
          (printf :: String -> String -> String -> String -> String)
            "%s[%s, %s]"
            prim_name
            (show search_ty)
            (show $ A.getFailProb eps)
    let all_params =
          Compiler.withTag CQPL.ParamOut [(ret, CPL.tbool), (x_out, search_ty)]
            ++ Compiler.withTag CQPL.ParamAux (zip pred_ancilla pred_aux_tys)
            ++ Compiler.withTag CQPL.ParamAux qsearch_ancilla

    return
      CQPL.ProcDef
        { CQPL.info_comment = info_comment
        , CQPL.proc_name = qsearch_proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = map (view _3) all_params
        , CQPL.proc_body =
            CQPL.ProcBodyU $
              CQPL.UProcBody
                { CQPL.uproc_param_names = map (view _1) all_params
                , CQPL.uproc_param_tags = map (view _2) all_params
                , CQPL.uproc_body_stmt = qsearch_body
                }
        }

-- ================================================================================
-- CQ Lowering
-- ================================================================================

-- | Run K grover iterations
groverK ::
  forall size.
  -- | number of rounds
  CPL.MetaParam size ->
  -- | the element and type to search for. @x : T@
  (Ident, CPL.VarType size) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> CQPL.UStmt size) ->
  CQPL.UStmt size
groverK k (x, x_ty) b mk_pred =
  CQPL.USeqS
    [ prepb
    , prepx
    , CQPL.URepeatS k grover_iterate
    , CQPL.adjoint prepb
    ]
 where
  unifX = CQPL.DistrU (CPL.UniformE x_ty)

  -- map b to |-> and x to uniform
  prepb, prepx :: CQPL.UStmt size
  prepb =
    CQPL.USeqS
      [ CQPL.UnitaryS [CQPL.Arg b] (CQPL.BasicGateU CQPL.XGate)
      , CQPL.UnitaryS [CQPL.Arg b] (CQPL.BasicGateU CQPL.HGate)
      ]
  prepx = CQPL.UnitaryS [CQPL.Arg x] unifX

  grover_iterate :: CQPL.UStmt size
  grover_iterate =
    CQPL.USeqS
      [ mk_pred x b
      , CQPL.UnitaryS [CQPL.Arg x] (CQPL.Adjoint unifX)
      , CQPL.UnitaryS [CQPL.Arg x] (CQPL.BasicGateU (CQPL.PhaseOnZero pi))
      , CQPL.UnitaryS [CQPL.Arg x] unifX
      ]

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
algoQSearch ::
  forall size prec m.
  ( Integral size
  , RealFloat prec
  , size ~ SizeT
  , Show size
  , Show prec
  , CPL.TypingReqs size
  , MonadError String m
  , MonadState (Compiler.LoweringCtx size) m
  ) =>
  -- | search elem type
  CPL.VarType size ->
  -- | number of classical samples
  size ->
  -- | max fail prob
  A.FailProb prec ->
  -- | grover_k caller: k, x, b
  (Either (CQPL.MetaParam size) Ident -> Ident -> Ident -> CQPL.Stmt size) ->
  -- | cqpl predicate caller
  (Ident -> Ident -> CQPL.Stmt size) ->
  -- | output bit
  Ident ->
  -- | output value
  Ident ->
  Compiler.ProcBuilderT size m ()
algoQSearch ty n_samples eps grover_k_caller pred_caller ok x = do
  not_done <- Compiler.allocLocalWithPrefix "not_done" CPL.tbool
  q_sum <- Compiler.allocLocalWithPrefix "Q_sum" j_type
  j <- Compiler.allocLocalWithPrefix "j" j_type
  j_lim <- Compiler.allocLocalWithPrefix "j_lim" j_type

  -- classical sampling
  when (n_samples /= 0) $ do
    let classicalSampling =
          CQPL.WhileKWithCondExpr (CQPL.MetaSize n_samples) not_done (notE (fromString ok)) $
            CQPL.SeqS
              [ CQPL.RandomS [x] (CPL.UniformE ty)
              , pred_caller x ok
              ]
    Compiler.addStmt classicalSampling

  -- quantum search

  -- one call and meas to grover with j iterations
  let quantumGroverOnce =
        CQPL.SeqS
          [ CQPL.RandomDynS j j_lim
          , CQPL.AssignS [q_sum] (fromString q_sum .+. fromString j)
          , CQPL.AssignS
              [not_done]
              (fromString not_done .&&. (fromString q_sum .<=. fromString j_lim))
          , CQPL.ifThenS
              not_done
              ( CQPL.SeqS
                  [ grover_k_caller (Right j) x ok
                  , pred_caller x ok
                  , CQPL.AssignS [not_done] (fromString not_done .&&. fromString ok)
                  ]
              )
          ]

  let quantumSamplingOneRound =
        CQPL.SeqS
          [ CQPL.AssignS [q_sum] (CPL.ConstE{CPL.val = CPL.FinV 0, CPL.ty = j_type})
          , CQPL.ForInArray
              { CQPL.loop_index = j_lim
              , CQPL.loop_index_ty = j_type
              , CQPL.loop_values = [CPL.ConstE (CPL.FinV $ fromIntegral v_j) j_type | v_j <- sampling_ranges]
              , CQPL.loop_body = quantumGroverOnce
              }
          ]

  let quantumSampling = CQPL.RepeatS (CQPL.MetaSize n_runs) quantumSamplingOneRound

  Compiler.addStmt quantumSampling
 where
  n = CPL.domainSize ty

  alpha = _QSearch_alpha
  lambda = 6 / 5

  sqrt_n :: Float
  sqrt_n = sqrt (fromIntegral n)

  n_runs, q_max :: SizeT
  n_runs = ceiling $ logBase 3 (1 / A.getFailProb eps)
  q_max = ceiling $ alpha * sqrt_n

  -- type for j and Q_sum
  j_type = CPL.Fin q_max

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

instance
  (RealFloat prec, Show prec) =>
  QuantumCompilePrim (QSearchCFNW SizeT prec) SizeT prec
  where
  compileQPrim (QSearchCFNW PrimSearch{search_kind, search_ty}) eps = do
    -- lowered unitary predicate
    (BooleanPredicate call_upred) <- view $ to mk_ucall
    (BooleanPredicate pred_aux_tys) <- view $ to uproc_aux_types

    ret_tys <- view $ to prim_ret_types
    (ret, x_out_param) <- case search_kind of
      SearchK -> do
        when (length ret_tys /= 2) $ throwError "search must return (bool, T)"
        b <- Compiler.newIdent "ret"
        x_out <- Compiler.newIdent "ret"
        pure (b, Just x_out)
      _ -> do
        when (length ret_tys /= 1) $ throwError "bool predicate must return single bool"
        b <- Compiler.newIdent "ret"
        return (b, Nothing)

    -- make the Grover_k uproc
    -- TODO this should ideally be done by algoQSearch, but requires a lot of aux information.
    uproc_grover_k_name <- Compiler.newIdent "Grover"
    upred_aux_vars <- replicateM (length pred_aux_tys) $ Compiler.newIdent "aux"
    grover_arg_name <- Compiler.newIdent "x"
    let meta_k = CPL.MetaName "k"
    let uproc_grover_k_body =
          groverK
            meta_k
            (grover_arg_name, search_ty)
            ret
            (\x b -> call_upred (map CQPL.Arg ([x, b] ++ upred_aux_vars)))
    let uproc_grover_k_params =
          Compiler.withTag CQPL.ParamInp [(grover_arg_name, search_ty)]
            ++ Compiler.withTag CQPL.ParamOut [(ret, CPL.tbool)]
            ++ Compiler.withTag CQPL.ParamAux (zip upred_aux_vars pred_aux_tys)
    let uproc_grover_k =
          CQPL.ProcDef
            { CQPL.info_comment = "Grover[...]"
            , CQPL.proc_name = uproc_grover_k_name
            , CQPL.proc_meta_params = ["k"]
            , CQPL.proc_param_types = map (view _3) uproc_grover_k_params
            , CQPL.proc_body =
                CQPL.ProcBodyU $
                  CQPL.UProcBody
                    { CQPL.uproc_param_names = map (view _1) uproc_grover_k_params
                    , CQPL.uproc_param_tags = map (view _2) uproc_grover_k_params
                    , CQPL.uproc_body_stmt = uproc_grover_k_body
                    }
            }
    Compiler.addProc uproc_grover_k

    let grover_k_caller k x b =
          CQPL.CallS
            { CQPL.fun = CQPL.UProcAndMeas uproc_grover_k_name
            , CQPL.meta_params = [k]
            , CQPL.args = [CQPL.Arg x, CQPL.Arg b]
            }

    (BooleanPredicate meas_upred) <- view $ to mk_meas
    let pred_caller x b = meas_upred [CQPL.Arg x, CQPL.Arg b]

    -- emit the QSearch algorithm
    let qsearch_params = case search_kind of
          SearchK -> [(ret, CPL.tbool), (fromJust x_out_param, search_ty)]
          _ -> [(ret, CPL.tbool)]

    let prim_name = case search_kind of AnyK -> "QAny"; AllK -> "QAll"; SearchK -> "QSearch"
    Compiler.buildProc prim_name [] qsearch_params $ do
      x_out <- case x_out_param of
        Just x -> pure x
        Nothing -> Compiler.allocLocalWithPrefix "s_result" search_ty
      algoQSearch search_ty 0 eps grover_k_caller pred_caller ret x_out

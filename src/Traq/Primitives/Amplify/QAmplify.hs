{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.QAmplify (
  QAmplify (..),

  -- * Query Formulas
  _FPAA_L,
  _WQSearch,
  _EQSearch,
) where

import Control.Monad (forM, when)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- | Quantum Amplitude Amplification.
newtype QAmplify size prec = QAmplify (Amplify size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (QAmplify size prec) = size
type instance PrecType (QAmplify size prec) = prec

type instance PrimFnShape (QAmplify size prec) = SamplerFn

instance Amplify size prec :<: QAmplify size prec

instance P.MapSize (QAmplify size prec) where
  type MappedSize (QAmplify size prec) size' = QAmplify size' prec
  mapSize f (QAmplify p) = QAmplify (P.mapSize f p)

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (QAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = QAmplify <$> parsePrimParams tp name
  printPrimParams (QAmplify prim) = printPrimParams prim

instance (P.TypingReqs size) => TypeCheckPrim (QAmplify size prec) size where
  inferRetTypesPrim (QAmplify p) = inferRetTypesPrim p

instance (P.EvalReqs size prec, Ord prec) => EvalPrim (QAmplify size prec) size prec where
  evalPrim (QAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Fixed-Point Amplitude Amplification.
_FPAA_L :: forall prec. (Floating prec) => A.FailProb prec -> prec -> prec
_FPAA_L eps p_min = acosh (1 / sqrt (A.getFailProb eps)) / acosh (1 / sqrt (1 - p_min))

instance (P.TypingReqs size, Floating prec) => UnitaryCostPrim (QAmplify size prec) size prec where
  unitaryQueryCosts (QAmplify Amplify{p_min}) eps = SamplerFn $ weakQueries $ _FPAA_L eps p_min
  unitaryExprCosts _ _ = Alg.zero

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 4 of https://arxiv.org/abs/2203.04975
-}
_WQSearch_alpha :: (Floating prec) => prec
_WQSearch_alpha = 9.2

_WQSearch_N_Runs :: forall prec. (Floating prec) => A.FailProb prec -> prec
_WQSearch_N_Runs eps = logBase 3 (1 / A.getFailProb eps)

_WQSearch_Q_Max :: forall prec. (Floating prec) => prec -> prec
_WQSearch_Q_Max p_min = _WQSearch_alpha / sqrt p_min

_WQSearch :: forall prec. (Floating prec) => A.FailProb prec -> prec -> prec
_WQSearch eps p_min = _WQSearch_N_Runs eps * _WQSearch_Q_Max p_min

-- | Eq. 3 of https://arxiv.org/abs/2203.04975
_F :: forall prec. (Floating prec, Ord prec) => prec -> prec
_F p_good
  | p_good >= 0.25 = 2.0344
  | otherwise = _WQSearch_alpha / (3 * sqrt p_good)

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 2 of https://arxiv.org/abs/2203.04975
-}
_EQSearch :: forall prec. (Floating prec, Ord prec) => A.FailProb prec -> prec -> prec -> prec
_EQSearch eps p_min p_good
  | p_good == 0 = _WQSearch eps p_min
  | otherwise = _F p_good * (1 + 1 / (1 - term))
 where
  term = _F p_good * sqrt p_min / _WQSearch_alpha

instance (P.TypingReqs size, A.SizeToPrec size prec, Floating prec) => QuantumHavocCostPrim (QAmplify size prec) size prec where
  quantumQueryCostsUnitary (QAmplify Amplify{p_min}) eps = SamplerFn $ strongQueries $ _WQSearch eps p_min
  quantumQueryCostsQuantum _ _ = SamplerFn 0

  quantumExprCosts = Alg.zero

instance (P.EvalReqs size prec, Floating prec, Ord prec) => QuantumExpCostPrim (QAmplify size prec) size prec where
  quantumExpQueryCostsUnitary (QAmplify Amplify{p_min}) eps (SamplerFn eval_sample) = SamplerFn $ strongQueries $ _EQSearch eps p_min p_good
   where
    mu = eval_sample []
    p_good = Prob.probabilityOf success mu

    -- check if sampling produced a good sample.
    success [b_val, _] = P.valueToBool b_val
    success _ = error "invalid predicate output"

  quantumExpQueryCostsQuantum _ _ _ = SamplerFn []

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Unitary Compilation
-- --------------------------------------------------------------------------------

instance (Floating prec, RealFrac prec) => UnitaryCompilePrim (QAmplify size prec) size prec where
  compileUPrim (QAmplify Amplify{p_min}) eps = do
    -- return vars and types
    rets <- view $ to ret_vars
    let b = head rets
    ret_tys <- forM rets $ \x -> do
      mty <- use $ P._typingCtx . Ctx.at x
      maybeWithError "" mty

    -- sampler
    (SamplerFn call_upred) <- view $ to mk_ucall
    (SamplerFn pred_aux_tys) <- view $ to uproc_aux_types

    -- parameters
    let l = ceiling ((_FPAA_L eps p_min - 1) / 2.0) :: Int
    let _L = 2 * l + 1 :: Int

    let acot x = atan (1 / x) :: Double
    let gamma = acosh ((1.0 / fromIntegral _L) * cosh (1.0 / sqrt (A.getFailProb eps))) :: prec
    let alphas = [2.0 * acot (tan (2 * pi * fromIntegral i / fromIntegral _L) * (1.0 - realToFrac gamma ** 2)) | i <- [1 .. l]]
    let betas = map negate $ reverse alphas

    -- algorithm
    qamplify_proc_name <- lift $ Compiler.newIdent "UAmplify"
    pred_aux <- lift $ mapM Compiler.allocAncilla pred_aux_tys

    let all_params =
          zip3 [] (repeat CQPL.ParamInp) []
            ++ zip3 rets (repeat CQPL.ParamOut) ret_tys
            ++ zip3 pred_aux (repeat CQPL.ParamAux) pred_aux_tys

    let sampler_call = call_upred (map CQPL.Arg (rets ++ pred_aux))
    let uproc_body_stmt =
          CQPL.USeqS $
            sampler_call
              : concat
                [ [ CQPL.UnitaryS [CQPL.Arg b] $ CQPL.BasicGateU $ CQPL.Rz beta
                  , CQPL.adjoint sampler_call
                  , CQPL.UnitaryS (map CQPL.Arg (rets ++ pred_aux)) $ CQPL.BasicGateU $ CQPL.PhaseOnZero (-alpha)
                  , sampler_call
                  ]
                | (alpha, beta) <- zip alphas betas
                ]

    return
      CQPL.ProcDef
        { CQPL.info_comment = ""
        , CQPL.proc_name = qamplify_proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = map (view _3) all_params
        , CQPL.proc_body =
            CQPL.ProcBodyU $
              CQPL.UProcBody
                { CQPL.uproc_param_names = map (view _1) all_params
                , CQPL.uproc_param_tags = map (view _2) all_params
                , CQPL.uproc_body_stmt
                }
        }

-- --------------------------------------------------------------------------------
-- CQ Compilation
-- --------------------------------------------------------------------------------

-- | Run K grover iterations
mkGroverK ::
  forall ext size prec m.
  ( m ~ PrimCompileMonad ext (QAmplify size prec)
  , size ~ SizeType ext
  ) =>
  m (CQPL.ProcDef size)
mkGroverK = do
  meta_k <- Compiler.newIdent "k"

  rets <- view $ to ret_vars
  let b = head rets
  ret_tys <- forM rets $ \x ->
    use (P._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

  Compiler.buildUProc "Grover" [meta_k] (zip rets ret_tys) $ do
    (SamplerFn mk_sampler_call) <- view $ to mk_ucall
    (SamplerFn aux_tys) <- view $ to uproc_aux_types
    aux_vars <- mapM Compiler.allocLocal aux_tys

    let sampler_call = mk_sampler_call (map CQPL.Arg (rets ++ aux_vars))

    Compiler.addUStmt sampler_call
    Compiler.withUStmt (CQPL.URepeatS (P.MetaName meta_k)) $
      -- grover_iteration
      mapM_
        Compiler.addUStmt
        [ CQPL.UnitaryS [CQPL.Arg b] $ CQPL.BasicGateU CQPL.ZGate
        , CQPL.adjoint sampler_call
        , CQPL.UnitaryS (map CQPL.Arg (rets ++ aux_vars)) $ CQPL.BasicGateU (CQPL.PhaseOnZero pi)
        , sampler_call
        ]

-- compute the limits for sampling `j` in each iteration.
qamplifySamplingRanges :: forall prec. (RealFloat prec) => prec -> [SizeT]
qamplifySamplingRanges p_min = go q_max js
 where
  q_max :: SizeT
  q_max = ceiling $ _WQSearch_Q_Max p_min

  lambda :: prec
  lambda = 6 / 5

  go :: SizeT -> [SizeT] -> [SizeT]
  go _ [] = []
  go lim (x : _) | x > lim = []
  go lim (x : xs) = x : go (lim - x) xs

  js :: [SizeT]
  js = map floor js_f

  js_f :: [prec]
  js_f = lambda : map nxt js_f

  nxt :: prec -> prec
  nxt m = min (lambda * m) (1 / sqrt p_min)

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
buildQAmplify ::
  forall ext prec m.
  ( RealFloat prec
  , SizeType ext ~ SizeT
  , m ~ Compiler.ProcBuilderT SizeT (PrimCompileMonad ext (QAmplify SizeT prec))
  ) =>
  -- | n_samples: number of classical samples
  SizeT ->
  -- | ret vars
  [Ident] ->
  -- | ret types
  [P.VarType SizeT] ->
  -- | eps: max fail prob
  A.FailProb prec ->
  -- | p_min: min success probability of sampler
  prec ->
  m ()
buildQAmplify n_samples rets _ret_tys eps p_min = do
  let b = head rets

  -- flag
  not_done <- Compiler.allocLocalWithPrefix "not_done" P.tbool
  Compiler.addStmt $ CQPL.AssignS [not_done] (P.ConstE (P.FinV 0) P.tbool)

  -- classical sampling
  SamplerFn mkSamplerCCall <- view $ to mk_call
  let sampler_call_c = mkSamplerCCall (map CQPL.Arg rets)

  when (n_samples /= 0) $
    Compiler.addStmt $
      CQPL.WhileKWithCondExpr
        (CQPL.MetaSize n_samples)
        not_done
        (P.notE (P.VarE b))
        sampler_call_c

  -- quantum iterations
  uproc_grover_k <- lift $ withSandbox mkGroverK
  lift $ Compiler.addProc uproc_grover_k

  let j_type = P.Fin (ceiling $ _WQSearch_Q_Max p_min) -- type for j and Q_sum
  q_sum <- Compiler.allocLocalWithPrefix "Q_sum" j_type
  j <- Compiler.allocLocalWithPrefix "j" j_type
  j_lim <- Compiler.allocLocalWithPrefix "j_lim" j_type

  let n_runs = ceiling $ _WQSearch_N_Runs eps

  Compiler.withStmt (CQPL.RepeatS (CQPL.MetaSize n_runs)) $ do
    Compiler.addStmt $ CQPL.AssignS [q_sum] (P.ConstE{P.val = P.FinV 0, P.ty = j_type})

    let sampling_ranges = qamplifySamplingRanges p_min
    Compiler.withStmt
      ( \s ->
          CQPL.ForInArray
            { CQPL.loop_index = j_lim
            , CQPL.loop_index_ty = j_type
            , CQPL.loop_values = [P.ConstE (P.FinV v_j) j_type | v_j <- sampling_ranges]
            , CQPL.loop_body = s
            }
      )
      $ do
        Compiler.addStmt $ CQPL.RandomDynS j j_lim
        Compiler.addStmt $ CQPL.AssignS [q_sum] (P.BinOpE P.AddOp (P.VarE q_sum) (P.VarE j))
        Compiler.addStmt $ CQPL.AssignS [not_done] (P.VarE not_done P..&&. (P.VarE q_sum P..<=. P.VarE j_lim))
        Compiler.withStmt (CQPL.ifThenS not_done) $ do
          Compiler.addStmt $
            CQPL.CallS
              { CQPL.fun = CQPL.UProcAndMeas (CQPL.proc_name uproc_grover_k)
              , CQPL.meta_params = [Right j]
              , CQPL.args = map CQPL.Arg rets
              }
          Compiler.addStmt $ CQPL.AssignS [not_done] (P.VarE not_done P..&&. P.notE (P.VarE b))

instance (RealFloat prec) => QuantumCompilePrim (QAmplify SizeT prec) SizeT prec where
  compileQPrim (QAmplify Amplify{p_min}) eps = do
    rets <- view $ to ret_vars
    ret_tys <- forM rets $ \x ->
      use (P._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

    Compiler.buildProc "QAmplify" [] (zip rets ret_tys) $ do
      buildQAmplify 0 rets ret_tys eps p_min

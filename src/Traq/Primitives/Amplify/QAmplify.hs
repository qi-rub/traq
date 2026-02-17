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

import Control.Monad (forM, replicateM)
import Control.Monad.Trans (lift)
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
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

-- | Quantum Amplitude Amplification.
newtype QAmplify sizeT precT = QAmplify (Amplify sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (QAmplify sizeT precT) = sizeT
type instance PrecType (QAmplify sizeT precT) = precT

type instance PrimFnShape (QAmplify size prec) = SamplerFn

instance Amplify sizeT precT :<: QAmplify sizeT precT

instance P.MapSize (QAmplify size prec) where
  type MappedSize (QAmplify size prec) size' = QAmplify size' prec
  mapSize f (QAmplify p) = QAmplify (P.mapSize f p)

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (QAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = QAmplify <$> parsePrimParams tp name
  printPrimParams (QAmplify prim) = printPrimParams prim

instance (P.TypingReqs sizeT) => TypeCheckPrim (QAmplify sizeT precT) sizeT where
  inferRetTypesPrim (QAmplify p) = inferRetTypesPrim p

instance (P.EvalReqs sizeT precT, Ord precT) => EvalPrim (QAmplify sizeT precT) sizeT precT where
  evalPrim (QAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Fixed-Point Amplitude Amplification.
_FPAA_L :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_FPAA_L eps p_min = acosh (1 / sqrt (A.getFailProb eps)) / acosh (1 / sqrt (1 - p_min))

instance (P.TypingReqs size, Floating prec) => UnitaryCostPrim (QAmplify size prec) size prec where
  unitaryQueryCosts (QAmplify Amplify{p_min}) eps = SamplerFn $ weakQueries $ _FPAA_L eps p_min
  unitaryExprCosts _ _ = Alg.zero

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 4 of https://arxiv.org/abs/2203.04975
-}
_WQSearch :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_WQSearch eps p_min = alpha * log (1 / A.getFailProb eps) / sqrt p_min
 where
  alpha = 9.2

-- | Eq. 3 of https://arxiv.org/abs/2203.04975
_F :: forall precT. (Floating precT, Ord precT) => precT -> precT
_F p_good
  | p_good >= 0.25 = 2.0344
  | otherwise = alpha / (3 * sqrt p_good)
 where
  alpha = 9.2

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 2 of https://arxiv.org/abs/2203.04975
-}
_EQSearch :: forall precT. (Floating precT, Ord precT) => A.FailProb precT -> precT -> precT -> precT
_EQSearch eps p_min p_good
  | p_good == 0 = _WQSearch eps p_min
  | otherwise = _F p_good * (1 + 1 / (1 - term))
 where
  term = _F p_good * sqrt p_min / alpha
  alpha = 9.2

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
  proc_name <- Compiler.newIdent "Grover"
  meta_k <- Compiler.newIdent "k"

  rets <- view $ to ret_vars
  let b = head rets
  ret_tys <- forM rets $ \x -> do
    error "return type of x"

  (SamplerFn mk_sampler_call) <- view $ to mk_ucall
  (SamplerFn aux_tys) <- view $ to uproc_aux_types
  aux_vars <- replicateM (length aux_tys) $ Compiler.newIdent "aux"

  let sampler_call = mk_sampler_call (map CQPL.Arg (rets ++ aux_vars))

  let grover_iteration =
        CQPL.USeqS
          [ CQPL.UnitaryS [CQPL.Arg b] $ CQPL.BasicGateU CQPL.ZGate
          , CQPL.adjoint sampler_call
          , CQPL.UnitaryS (map CQPL.Arg (rets ++ aux_vars)) $ CQPL.BasicGateU (CQPL.PhaseOnZero pi)
          , sampler_call
          ]

  let uproc_body_stmt =
        CQPL.USeqS
          [ sampler_call
          , CQPL.URepeatS{n_iter = P.MetaName meta_k, uloop_body = grover_iteration}
          ]

  let params =
        Compiler.withTag CQPL.ParamOut (zip rets ret_tys)
          ++ Compiler.withTag CQPL.ParamAux (zip aux_vars aux_tys)
  return $
    CQPL.ProcDef
      { CQPL.info_comment = ""
      , CQPL.proc_name
      , CQPL.proc_meta_params = [meta_k]
      , CQPL.proc_param_types = map (view _3) params
      , CQPL.proc_body =
          CQPL.ProcBodyU $
            CQPL.UProcBody
              { CQPL.uproc_param_names = map (view _1) params
              , CQPL.uproc_param_tags = map (view _2) params
              , CQPL.uproc_body_stmt
              }
      }

{-
uproc QAmplify(b, y) do {
  not_done := 0 : Bool;
  repeat $N_\text{runs}$ {
    Q_sum := 0 : Fin<$Q_\text{max}$>;
    for j_lim : Fin<$Q_\text{max}$> in $\vec{J}$ {
      j :=$\texttt{\$}$ [1 .. j_lim] : Fin<$Q_\text{max}$>;
      Q_sum := Q_sum + j;
      not_done := not_done and (Q_sum <= j_lim);
      if (not_done) {
        meas grover$_j$(b, y); // run the grover iterations
        not_done := not_done and (not b);
      }
    }
  }
}
-}

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
algoQSearch ::
  forall ext sizeT precT m.
  ( Integral sizeT
  , RealFloat precT
  , sizeT ~ SizeT
  , Show sizeT
  , Show precT
  , P.TypingReqs sizeT
  , SizeType ext ~ sizeT
  , m ~ PrimCompileMonad ext (QAmplify sizeT precT)
  ) =>
  -- | search elem type
  P.VarType sizeT ->
  -- | number of classical samples
  sizeT ->
  -- | max fail prob
  A.FailProb precT ->
  -- | grover_k caller: k, x, b
  (Either (CQPL.MetaParam sizeT) Ident -> Ident -> Ident -> CQPL.Stmt sizeT) ->
  -- | cqpl predicate caller
  (Ident -> Ident -> CQPL.Stmt sizeT) ->
  -- | output bit
  Ident ->
  -- | output value
  Ident ->
  -- | the generated QSearch procedure: body stmts and local vars
  m ()
algoQSearch ty n_samples eps grover_k_caller pred_caller ok x = do
  not_done <- allocReg "not_done" P.tbool
  q_sum <- allocReg "Q_sum" j_type
  j <- allocReg "j" j_type
  j_lim <- allocReg "j_lim" j_type

  -- classical sampling
  when (n_samples /= 0) $ do
    let classicalSampling =
          CQPL.WhileKWithCondExpr (CQPL.MetaSize n_samples) not_done (notE (fromString ok)) $
            CQPL.SeqS
              [ CQPL.RandomS [x] (P.UniformE ty)
              , pred_caller x ok
              ]
    writeElemAt _1 classicalSampling

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
          [ CQPL.AssignS [q_sum] (P.ConstE{P.val = P.FinV 0, P.ty = j_type})
          , CQPL.ForInArray
              { CQPL.loop_index = j_lim
              , CQPL.loop_index_ty = j_type
              , CQPL.loop_values = [P.ConstE (P.FinV $ fromIntegral v_j) j_type | v_j <- sampling_ranges]
              , CQPL.loop_body = quantumGroverOnce
              }
          ]

  let quantumSampling = CQPL.RepeatS (CQPL.MetaSize n_runs) quantumSamplingOneRound

  writeElemAt _1 quantumSampling
 where
  n = P.domainSize ty

  alpha = _QSearch_alpha
  lambda = 6 / 5

  sqrt_n :: Float
  sqrt_n = sqrt (fromIntegral n)

  n_runs, q_max :: SizeT
  n_runs = ceiling $ logBase 3 (1 / A.getFailProb eps)
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

-- instance
--   (RealFloat prec, Show prec) =>
--   QuantumCompilePrim (QSearchCFNW SizeT prec) SizeT prec
--   where
--   compileQPrim (QSearchCFNW PrimSearch{search_kind, search_ty}) eps = do
--     -- lowered unitary predicate
--     (BooleanPredicate call_upred) <- view $ to mk_ucall
--     (BooleanPredicate pred_aux_tys) <- view $ to uproc_aux_types

--     (ret, x_out) <- case search_kind of
--       SearchK -> do
--         view (to ret_vars) >>= \case
--           [b, x_out] -> pure (b, x_out)
--           _ -> throwError "search must return (bool, T)"
--       _ -> do
--         b <-
--           view (to ret_vars) >>= \case
--             [b] -> pure b
--             _ -> throwError "bool predicate must return single bool"
--         x_out <- lift $ Compiler.allocAncillaWithPref "s_result" search_ty
--         return (b, x_out)

--     -- make the Grover_k uproc
--     -- TODO this should ideally be done by algoQSearch, but requires a lot of aux information.
--     uproc_grover_k_name <- Compiler.newIdent "Grover"
--     upred_aux_vars <- replicateM (length pred_aux_tys) $ Compiler.newIdent "aux"
--     grover_arg_name <- Compiler.newIdent "x"
--     let meta_k = P.MetaName "k"
--     let uproc_grover_k_body =
--           groverK
--             meta_k
--             (grover_arg_name, search_ty)
--             ret
--             (\x b -> call_upred (map CQPL.Arg ([x, b] ++ upred_aux_vars)))
--     let uproc_grover_k_params =
--           Compiler.withTag CQPL.ParamInp [(grover_arg_name, search_ty)]
--             ++ Compiler.withTag CQPL.ParamOut [(ret, P.tbool)]
--             ++ Compiler.withTag CQPL.ParamAux (zip upred_aux_vars pred_aux_tys)
--     let uproc_grover_k =
--           CQPL.ProcDef
--             { CQPL.info_comment = "Grover[...]"
--             , CQPL.proc_name = uproc_grover_k_name
--             , CQPL.proc_meta_params = ["k"]
--             , CQPL.proc_param_types = map (view _3) uproc_grover_k_params
--             , CQPL.proc_body =
--                 CQPL.ProcBodyU $
--                   CQPL.UProcBody
--                     { CQPL.uproc_param_names = map (view _1) uproc_grover_k_params
--                     , CQPL.uproc_param_tags = map (view _2) uproc_grover_k_params
--                     , CQPL.uproc_body_stmt = uproc_grover_k_body
--                     }
--             }
--     Compiler.addProc uproc_grover_k

--     let grover_k_caller k x b =
--           CQPL.CallS
--             { CQPL.fun = CQPL.UProcAndMeas uproc_grover_k_name
--             , CQPL.meta_params = [k]
--             , CQPL.args = [CQPL.Arg x, CQPL.Arg b]
--             }

--     -- emit the QSearch algorithm
--     let qsearch_params =
--           case search_kind of
--             SearchK -> [(ret, P.tbool), (x_out, search_ty)]
--             _ -> [(ret, P.tbool)]

--     (BooleanPredicate meas_upred) <- view $ to mk_meas
--     let pred_caller x b = meas_upred [x, b]

--     (qsearch_body, qsearch_local_vars) <-
--       lift $
--         execWriterT $
--           algoQSearch search_ty 0 eps grover_k_caller pred_caller ret x_out

--     let prim_name = (case search_kind of AnyK -> "QAny"; AllK -> "QAll"; SearchK -> "QSearch")
--     qsearch_proc_name <- Compiler.newIdent prim_name
--     return
--       CQPL.ProcDef
--         { CQPL.info_comment = printf "%s[%s]" prim_name (show $ A.getFailProb eps)
--         , CQPL.proc_name = qsearch_proc_name
--         , CQPL.proc_meta_params = []
--         , CQPL.proc_param_types = map snd qsearch_params
--         , CQPL.proc_body =
--             CQPL.ProcBodyC $
--               CQPL.CProcBody
--                 { CQPL.cproc_param_names = map fst qsearch_params
--                 , CQPL.cproc_local_vars = qsearch_local_vars ++ [(x_out, search_ty) | search_kind /= SearchK]
--                 , CQPL.cproc_body_stmt = CQPL.SeqS qsearch_body
--                 }
--         }

instance (Floating prec, RealFrac prec) => QuantumCompilePrim (QAmplify size prec) size prec where
  compileQPrim (QAmplify Amplify{p_min}) eps = do
    -- return vars and types
    rets <- view $ to ret_vars
    ret_tys <- forM rets $ \x -> do
      mty <- use $ P._typingCtx . Ctx.at x
      maybeWithError "" mty

    -- sampler
    (SamplerFn call_upred) <- view $ to mk_ucall
    (SamplerFn pred_aux_tys) <- view $ to uproc_aux_types

    -- algorithm
    qamplify_proc_name <- lift $ Compiler.newIdent "QAmplify"
    let args = zip rets ret_tys
    let local_vars = []
    let cproc_body_stmt = CQPL.CommentS "TODO"

    return
      CQPL.ProcDef
        { CQPL.info_comment = ""
        , CQPL.proc_name = qamplify_proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = map snd args
        , CQPL.proc_body =
            CQPL.ProcBodyC $
              CQPL.CProcBody
                { CQPL.cproc_param_names = map fst args
                , CQPL.cproc_local_vars = local_vars
                , CQPL.cproc_body_stmt
                }
        }

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

import Control.Monad (forM, replicateM, when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), censor)
import Data.Bifunctor (second)
import Data.String (IsString (..))
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
_WQSearch_alpha :: (Floating prec) => prec
_WQSearch_alpha = 9.2

_WQSearch_N_Runs :: forall prec. (Floating prec) => A.FailProb prec -> prec
_WQSearch_N_Runs eps = logBase 3 (1 / A.getFailProb eps)

_WQSearch_Q_Max :: forall prec. (Floating prec) => prec -> prec
_WQSearch_Q_Max p_min = _WQSearch_alpha / sqrt p_min

_WQSearch :: forall precT. (Floating precT) => A.FailProb precT -> precT -> precT
_WQSearch eps p_min = _WQSearch_N_Runs eps * _WQSearch_Q_Max p_min

-- | Eq. 3 of https://arxiv.org/abs/2203.04975
_F :: forall precT. (Floating precT, Ord precT) => precT -> precT
_F p_good
  | p_good >= 0.25 = 2.0344
  | otherwise = _WQSearch_alpha / (3 * sqrt p_good)

{- | Cost of quantum search adapted to general amplitude amplification.
Eq. 2 of https://arxiv.org/abs/2203.04975
-}
_EQSearch :: forall precT. (Floating precT, Ord precT) => A.FailProb precT -> precT -> precT -> precT
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
  proc_name <- Compiler.newIdent "Grover"
  meta_k <- Compiler.newIdent "k"

  rets <- view $ to ret_vars
  let b = head rets
  ret_tys <- forM rets $ \x ->
    use (P._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

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

type LocalVars = [(Ident, P.VarType SizeT)]
type AlgoMonad ext prec =
  WriterT (LocalVars, [CQPL.Stmt SizeT]) (PrimCompileMonad ext (QAmplify SizeT prec))

allocLocal :: (m ~ AlgoMonad ext prec) => Ident -> P.VarType SizeT -> m Ident
allocLocal pref ty = do
  x <- Compiler.newIdent pref
  writeElemAt _1 (x, ty)
  return x

addStmt :: (m ~ AlgoMonad ext prec) => CQPL.Stmt SizeT -> m ()
addStmt = writeElemAt _2

withStmt :: (m ~ AlgoMonad ext prec) => (CQPL.Stmt SizeT -> CQPL.Stmt SizeT) -> m a -> m a
withStmt f = censor (second f')
 where
  f' ss = [f (CQPL.SeqS ss)]

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
  , Show prec
  , SizeType ext ~ SizeT
  , m ~ AlgoMonad ext prec
  ) =>
  -- | n_samples: number of classical samples
  SizeT ->
  -- | eps: max fail prob
  A.FailProb prec ->
  -- | p_min: min success probability of sampler
  prec ->
  m ()
buildQAmplify n_samples eps p_min = do
  -- rets
  rets <- view $ to ret_vars
  let b = head rets

  -- flag
  not_done <- allocLocal "not_done" P.tbool
  addStmt $ CQPL.AssignS [not_done] (P.ConstE 0 P.tbool)

  -- classical sampling
  SamplerFn mkSamplerCCall <- view $ to mk_call
  let sampler_call_c = mkSamplerCCall (map CQPL.Arg rets)

  when (n_samples /= 0) $
    addStmt $
      CQPL.WhileKWithCondExpr
        (CQPL.MetaSize n_samples)
        not_done
        (P.notE (P.VarE b))
        sampler_call_c

  -- quantum iterations
  uproc_grover_k <- lift $ withSandbox mkGroverK
  lift $ Compiler.addProc uproc_grover_k

  let j_type = P.Fin (ceiling $ _WQSearch_Q_Max p_min) -- type for j and Q_sum
  q_sum <- allocLocal "Q_sum" j_type
  j <- allocLocal "j" j_type
  j_lim <- allocLocal "j_lim" j_type

  let n_runs = ceiling $ _WQSearch_N_Runs eps

  withStmt (CQPL.RepeatS (CQPL.MetaSize n_runs)) $ do
    addStmt $ CQPL.AssignS [q_sum] (P.ConstE{P.val = P.FinV 0, P.ty = j_type})

    let sampling_ranges = qamplifySamplingRanges p_min
    withStmt
      ( \s ->
          CQPL.ForInArray
            { CQPL.loop_index = j_lim
            , CQPL.loop_index_ty = j_type
            , CQPL.loop_values = [P.ConstE (P.FinV v_j) j_type | v_j <- sampling_ranges]
            , CQPL.loop_body = s
            }
      )
      $ do
        addStmt $ CQPL.RandomDynS j j_lim
        addStmt $ CQPL.AssignS [q_sum] (P.BinOpE P.AddOp (P.VarE q_sum) (P.VarE j))
        addStmt $ CQPL.AssignS [not_done] (error "not_done && (q_sum <= j_lim)")
        withStmt (CQPL.ifThenS not_done) $ do
          addStmt $ error "meas grover[#j](rets)"
          addStmt $ CQPL.AssignS [not_done] (error "not_done && (not b)")

instance (Floating prec, RealFrac prec) => QuantumCompilePrim (QAmplify SizeT prec) SizeT prec where
  compileQPrim (QAmplify Amplify{p_min}) eps = do
    rets <- view $ to ret_vars
    ret_tys <- forM rets $ \x ->
      use (P._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

    proc_name <- Compiler.newIdent "QAmplify"
    ((), (local_vars, body)) <- runWriterT $ withSandbox $ buildQAmplify 0 eps p_min

    return $
      CQPL.ProcDef
        { CQPL.info_comment = ""
        , CQPL.proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = ret_tys
        , CQPL.proc_body =
            CQPL.ProcBodyC $
              CQPL.CProcBody
                { CQPL.cproc_param_names = rets
                , CQPL.cproc_local_vars = local_vars
                , CQPL.cproc_body_stmt = CQPL.SeqS body
                }
        }

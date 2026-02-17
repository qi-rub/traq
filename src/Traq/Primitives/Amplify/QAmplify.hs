{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.QAmplify (
  QAmplify (..),

  -- * Query Formulas
  _FPAA_L,
  _WQSearch,
  _EQSearch,
) where

import Control.Monad (forM)
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

instance (Floating prec, RealFrac prec) => UnitaryCompilePrim (QAmplify size prec) size prec where
  compileUPrim (QAmplify Amplify{p_min}) eps = do
    -- return vars and types
    rets <- view $ to ret_vars
    ret_tys <- forM rets $ \x -> do
      mty <- use $ P._typingCtx . Ctx.at x
      maybeWithError "" mty

    -- sampler
    (SamplerFn call_upred) <- view $ to mk_ucall
    (SamplerFn pred_aux_tys) <- view $ to uproc_aux_types

    -- parameters
    let l = ceiling ((_FPAA_L eps p_min - 1) / 2.0) :: Int

    -- algorithm
    qamplify_proc_name <- lift $ Compiler.newIdent "UAmplify"
    pred_aux <- lift $ mapM Compiler.allocAncilla pred_aux_tys

    let all_params =
          zip3 [] (repeat CQPL.ParamInp) []
            ++ zip3 rets (repeat CQPL.ParamOut) ret_tys
            ++ zip3 pred_aux (repeat CQPL.ParamAux) pred_aux_tys

    let uproc_body_stmt = CQPL.UCommentS "TODO"

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

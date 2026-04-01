{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Amplify.CAmplify (
  CAmplify (..),

  -- * Symbolic formulas
  _QMax,
  _EQ,
)
where

import Control.Monad (replicateM)
import Control.Monad.Except (throwError)
import GHC.Generics (Generic)
import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.QPL as QPL

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

-- | Classical (probabilistic) bounded repetition.
newtype CAmplify size prec = CAmplify (Amplify size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (CAmplify size prec) = size
type instance PrecType (CAmplify size prec) = prec

type instance PrimFnShape (CAmplify size prec) = SamplerFn

instance Amplify size prec :<: CAmplify size prec

instance CPL.MapSize (CAmplify size prec) where
  type MappedSize (CAmplify size prec) size' = CAmplify size' prec
  mapSize f (CAmplify p) = CAmplify (CPL.mapSize f p)

instance CPL.MapPrec (CAmplify size prec) where
  type MappedPrec (CAmplify size prec) prec' = CAmplify size prec'
  mapPrec f (CAmplify p) = CAmplify (CPL.mapPrec f p)

-- Inherited instances
instance (Show prec, Fractional prec) => SerializePrim (CAmplify size prec) where
  primNames = ["amplify"]

  parsePrimParams tp name = CAmplify <$> parsePrimParams tp name
  printPrimParams (CAmplify prim) = printPrimParams prim

instance (CPL.TypingReqs size) => TypeCheckPrim (CAmplify size prec) size where
  inferRetTypesPrim (CAmplify p) = inferRetTypesPrim p

instance (CPL.EvalReqs size prec, Ord prec) => EvalPrim (CAmplify size prec) size prec where
  evalPrim (CAmplify p) = evalPrim p

-- ================================================================================
-- Costs
-- ================================================================================

-- | Maximum queries
_QMax :: forall prec. (Floating prec) => A.FailProb prec -> prec -> prec
_QMax eps p_min = logBase (1 / (1 - p_min)) (1 / A.getFailProb eps)

-- | Expected queries
_EQ :: forall prec. (Floating prec, Ord prec) => A.FailProb prec -> prec -> prec -> prec
_EQ eps p_min p_good
  | p_good >= p_min = 1 / p_good
  | p_good == 0 = _QMax eps p_min
  | otherwise = error "invalid case: 0 < p_good < p_min"

instance (CPL.TypingReqs size, Floating prec) => UnitaryCostPrim (CAmplify size prec) size prec where
  unitaryQueryCosts (CAmplify Amplify{p_min}) eps = SamplerFn $ weakQueries $ _QMax eps p_min
  unitaryExprCosts = Alg.zero

instance (CPL.TypingReqs size, A.SizeToPrec size prec, Floating prec) => QuantumHavocCostPrim (CAmplify size prec) size prec where
  quantumQueryCostsQuantum (CAmplify Amplify{p_min}) eps = SamplerFn $ _QMax eps p_min

  -- no unitary cost for classical algo
  quantumQueryCostsUnitary _ _ = SamplerFn zeroQ

  quantumExprCosts = Alg.zero

instance (CPL.EvalReqs size prec, Floating prec, Ord prec) => QuantumExpCostPrim (CAmplify size prec) size prec where
  quantumExpQueryCostsQuantum (CAmplify Amplify{p_min}) eps (SamplerFn eval_sample) = SamplerFn [([], _EQ eps p_min p_succ)]
   where
    mu = eval_sample []
    p_succ = Prob.probabilityOf success mu

    -- check if sampling produced a good sample.
    success [b_val, _] = CPL.valueToBool b_val
    success _ = error "invalid predicate output"

  -- no unitary cost for classical algo
  quantumExpQueryCostsUnitary _ _ _ = SamplerFn zeroQ

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

instance (Floating prec, RealFrac prec, Integral size) => UnitaryCompilePrim (CAmplify size prec) size prec where
  compileUPrim (CAmplify Amplify{p_min}) eps = do
    ret_tys <- view $ to prim_ret_types

    (SamplerFn call_upred) <- view $ to mk_ucall
    (SamplerFn pred_aux_tys) <- view $ to uproc_aux_types

    let q_max = ceiling (_QMax eps p_min)

    rets <- replicateM (length ret_tys) $ Compiler.newIdent "ret"

    Compiler.buildUProc "UCAmplify" [] (zip rets ret_tys) $ do
      tmp <- mapM (Compiler.allocLocal . CPL.Arr q_max) ret_tys
      aux <- mapM (Compiler.allocLocal . CPL.Arr q_max) pred_aux_tys

      i <- Compiler.newIdent "i"
      Compiler.withUStmt (QPL.UForInDomainS i (CPL.Fin q_max) False) $ do
        let tmp_ix = map ((`QPL.ArrElemArg` CPL.MetaName i) . QPL.Arg) tmp
        let aux_ix = map ((`QPL.ArrElemArg` CPL.MetaName i) . QPL.Arg) aux
        Compiler.addUStmt $ call_upred (tmp_ix ++ aux_ix)

      Compiler.addUStmt $ QPL.UnitaryS (map QPL.Arg (tmp ++ rets)) (QPL.NamedGateU "Select")

instance (Floating prec, RealFrac prec, Integral size) => QuantumCompilePrim (CAmplify size prec) size prec where
  compileQPrim (CAmplify Amplify{p_min}) eps = do
    ret_tys <- view $ to prim_ret_types
    (flag_ty, sample_ty) <- case ret_tys of
      [b, t] -> return (b, t)
      _ -> throwError "typecheck failed"

    flag <- Compiler.newIdent "b"
    sample <- Compiler.newIdent "y"

    (SamplerFn call_pred) <- view $ to mk_call

    let q_max = ceiling (_QMax eps p_min)

    Compiler.buildProc "CAmplify" [] [(flag, flag_ty), (sample, sample_ty)] $ do
      Compiler.withStmt (QPL.RepeatS (QPL.MetaSize q_max)) $ do
        Compiler.addStmt $
          QPL.IfThenElseS
            { cond = flag
            , s_true = QPL.SkipS
            , s_false = call_pred [QPL.Arg flag, QPL.Arg sample]
            }

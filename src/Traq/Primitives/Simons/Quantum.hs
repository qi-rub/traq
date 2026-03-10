{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Simons.Quantum (
  -- * Primitive
  SimonsFindXorPeriod (..),

  -- * Query Formulas
  _SimonsQueries,
) where

import Control.Monad (forM, replicateM)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Prelude
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive and Query Cost Formulas
-- ================================================================================

{- |
Simon's Quantum Algorithm with an approximate promise ([1], Theorem 1)

References:

 1. [Breaking Symmetric Cryptosystems using Quantum Period Finding](https://arxiv.org/pdf/1602.05973)
-}
newtype SimonsFindXorPeriod size prec = SimonsFindXorPeriod (FindXorPeriod size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (SimonsFindXorPeriod size prec) = size
type instance PrecType (SimonsFindXorPeriod size prec) = prec

type instance PrimFnShape (SimonsFindXorPeriod size prec) = FindXorPeriodArg

instance CPL.MapSize (SimonsFindXorPeriod size prec) where
  type MappedSize (SimonsFindXorPeriod size prec) size' = (SimonsFindXorPeriod size' prec)
  mapSize f (SimonsFindXorPeriod p) = SimonsFindXorPeriod (CPL.mapSize f p)

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance FindXorPeriod size prec :<: SimonsFindXorPeriod size prec

instance IsA (FindXorPeriod size prec) (SimonsFindXorPeriod size prec)

instance (Show size) => SerializePrim (SimonsFindXorPeriod size Double) where
  primNames = ["findXorPeriod"]
  parsePrimParams tp s = SimonsFindXorPeriod <$> parsePrimParams tp s
  printPrimParams (SimonsFindXorPeriod prim) = printPrimParams prim

instance
  (CPL.TypingReqs size, Num prec, Ord prec, Show prec) =>
  TypeCheckPrim (SimonsFindXorPeriod size prec) size
  where
  inferRetTypesPrim (SimonsFindXorPeriod p) = inferRetTypesPrim p

-- ================================================================================
-- Cost Instances
-- ================================================================================

-- | Number of queries as described in Theorem 1.
_SimonsQueries ::
  forall size prec.
  (Floating prec, A.SizeToPrec size prec) =>
  -- | bitsize
  size ->
  -- | p_0: maximum probability of spurious collisions for non-period values.
  prec ->
  -- | maximum allowed failure probability.
  A.FailProb prec ->
  prec
_SimonsQueries n p0 eps = q + 1
 where
  {- Sketch:
    We need to pick @c@ such that @(2 * ((1 + p0)/2)^c)^n <= eps@ (see Theorem 1)
    ==> 2^n / eps <= (2 / (1+p0))^(cn)
    define q := cn (i.e. number of queries)
    ==> n + log_2 (1/eps) <= q * log_2(2 / (1+p0))
  -}

  q_num = A.sizeToPrec n + logBase 2 (1 / A.getFailProb eps)
  q_den = logBase 2 (2 / (1 + p0))
  q = q_num / q_den

instance
  (CPL.TypingReqs size, Floating prec, Ord prec, Show prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (SimonsFindXorPeriod size prec) size prec
  where
  unitaryQueryCosts prim eps =
    let FindXorPeriod{n, p_0} = extract prim :: FindXorPeriod size prec
     in FindXorPeriodArg{fun = strongQueries $ _SimonsQueries n p_0 eps}

  unitaryExprCosts _ _ = Alg.zero

-- | Same as unitary compilation.
instance
  (CPL.TypingReqs size, Floating prec, Ord prec, Show prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (SimonsFindXorPeriod size prec) size prec
  where
  quantumQueryCostsQuantum _ _ = FindXorPeriodArg{fun = 0}

  quantumQueryCostsUnitary prim eps =
    let FindXorPeriod{n, p_0} = extract prim :: FindXorPeriod size prec
     in FindXorPeriodArg{fun = strongQueries $ _SimonsQueries n p_0 eps}

  quantumExprCosts = Alg.zero

-- ================================================================================
-- Compiler
-- ================================================================================

simonsOneRound ::
  forall ext size prec m.
  (size ~ SizeType ext, m ~ PrimCompileMonad ext (SimonsFindXorPeriod size prec)) =>
  [CPL.VarType size] ->
  m (QPL.ProcDef size)
simonsOneRound arg_tys = do
  (FindXorPeriodArg call_upred) <- view $ to mk_ucall
  (FindXorPeriodArg pred_aux_tys) <- view $ to uproc_aux_types

  proc_name <- lift $ Compiler.newIdent "SimonOneRound_U"

  xs <- lift $ mapM (Compiler.allocAncillaWithPref "x") arg_tys
  ys <- lift $ mapM (Compiler.allocAncillaWithPref "y") arg_tys
  ys' <- lift $ mapM (Compiler.allocAncillaWithPref "yy") arg_tys
  aux <- lift $ mapM Compiler.allocAncilla pred_aux_tys

  let had_xs = QPL.USeqS [QPL.UnitaryS [QPL.Arg x] (QPL.DistrU $ CPL.UniformE t) | (x, t) <- zip xs arg_tys]
  let call_g = call_upred (map QPL.Arg (xs ++ ys ++ aux))
  let copy_out = QPL.USeqS [QPL.UnitaryS [QPL.Arg y, QPL.Arg y'] (QPL.BasicGateU QPL.COPY) | (y, y') <- zip ys ys']

  let body_compute = QPL.USeqS [had_xs, call_g]
  let uproc_body_stmt =
        QPL.USeqS
          [ body_compute
          , copy_out
          , QPL.adjoint body_compute
          ]

  return
    QPL.ProcDef
      { QPL.info_comment = ""
      , QPL.proc_name
      , QPL.proc_meta_params = []
      , QPL.proc_param_types = arg_tys ++ arg_tys ++ arg_tys ++ pred_aux_tys
      , QPL.proc_body =
          QPL.ProcBodyU $
            QPL.UProcBody
              { QPL.uproc_param_names = xs ++ ys ++ ys' ++ aux
              , QPL.uproc_param_tags =
                  replicate (length arg_tys) QPL.ParamOut
                    ++ replicate (length arg_tys) QPL.ParamAux
                    ++ replicate (length arg_tys) QPL.ParamAux
                    ++ replicate (length pred_aux_tys) QPL.ParamAux
              , QPL.uproc_body_stmt
              }
      }

instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (SimonsFindXorPeriod size prec) size prec
  where
  compileUPrim (SimonsFindXorPeriod FindXorPeriod{n, p_0}) eps = do
    arg_tys <- view $ to prim_ret_types
    rets <- replicateM (length arg_tys) $ Compiler.newIdent "ret"

    simons_uproc <- simonsOneRound arg_tys
    Compiler.addProc simons_uproc

    proc_name <- lift $ Compiler.newIdent "USimon"
    i <- lift $ Compiler.newIdent "i"

    let nq = floor $ _SimonsQueries n p_0 eps
    xts <- forM (simons_uproc & QPL.proc_param_types) $ \t -> do
      let t' = CPL.Arr nq t
      x <- lift $ Compiler.allocAncillaWithPref (proc_name ++ "_aux") t'
      pure (x, t')

    let uproc_body_stmt =
          QPL.USeqS
            [ QPL.UForInRangeS
                { QPL.iter_meta_var = i
                , QPL.iter_lim = CPL.MetaSize nq
                , QPL.uloop_body =
                    QPL.UCallS
                      { uproc_id = QPL.proc_name simons_uproc
                      , dagger = False
                      , qargs = map (\(x, _) -> QPL.ArrElemArg (QPL.Arg x) (CPL.MetaName i)) xts
                      }
                , dagger = False
                }
            , QPL.UCommentS $
                printf
                  "simon's post-processing: unitarily solve linear system: (%s) . (%s) = 0"
                  (PP.commaList rets)
                  (PP.commaList $ take (length arg_tys) $ map fst xts)
            ]

    return
      QPL.ProcDef
        { QPL.info_comment = ""
        , QPL.proc_name
        , QPL.proc_meta_params = []
        , QPL.proc_param_types = arg_tys ++ map snd xts
        , QPL.proc_body =
            QPL.ProcBodyU $
              QPL.UProcBody
                { QPL.uproc_param_names = rets ++ map fst xts
                , QPL.uproc_param_tags = replicate (length rets) QPL.ParamOut ++ replicate (length xts) QPL.ParamAux
                , QPL.uproc_body_stmt
                }
        }

instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  QuantumCompilePrim (SimonsFindXorPeriod size prec) size prec
  where
  compileQPrim (SimonsFindXorPeriod FindXorPeriod{n, p_0}) eps = do
    arg_tys <- view $ to prim_ret_types
    rets <- replicateM (length arg_tys) $ Compiler.newIdent "ret"

    simons_uproc <- simonsOneRound arg_tys
    Compiler.addProc simons_uproc

    proc_name <- lift $ Compiler.newIdent "QSimon"
    i <- lift $ Compiler.newIdent "i"

    let nq = floor $ _SimonsQueries n p_0 eps
    xts <- forM arg_tys $ \t -> do
      let t' = CPL.Arr nq t
      x <- lift $ Compiler.allocAncillaWithPref (proc_name ++ "__u") t'
      pure (x, t')

    let cproc_body_stmt =
          QPL.SeqS
            [ QPL.ForInRangeS
                { QPL.iter_meta_var = i
                , QPL.iter_lim = CPL.MetaSize nq
                , QPL.loop_body =
                    QPL.CallS
                      { fun = QPL.UProcAndMeas (QPL.proc_name simons_uproc)
                      , meta_params = []
                      , args = map (\(x, _) -> QPL.ArrElemArg (QPL.Arg x) (CPL.MetaName i)) xts
                      }
                }
            , QPL.CommentS $
                printf
                  "simon's post-processing: solve linear system: (%s) . (%s) = 0"
                  (PP.commaList rets)
                  (PP.commaList $ map fst xts)
            ]

    return
      QPL.ProcDef
        { QPL.info_comment = ""
        , QPL.proc_name
        , QPL.proc_meta_params = []
        , QPL.proc_param_types = arg_tys
        , QPL.proc_body =
            QPL.ProcBodyC $
              QPL.CProcBody
                { QPL.cproc_param_names = rets
                , QPL.cproc_local_vars = xts
                , QPL.cproc_body_stmt
                }
        }

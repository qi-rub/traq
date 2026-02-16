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

import Control.Monad (forM)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Subtyping

import qualified Traq.Analysis as P
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive and Query Cost Formulas
-- ================================================================================

{- |
Simon's Quantum Algorithm with an approximate promise ([1], Theorem 1)

References:

 1. [Breaking Symmetric Cryptosystems using Quantum Period Finding](https://arxiv.org/pdf/1602.05973)
-}
newtype SimonsFindXorPeriod sizeT precT = SimonsFindXorPeriod (FindXorPeriod sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (SimonsFindXorPeriod sizeT precT) = sizeT
type instance PrecType (SimonsFindXorPeriod sizeT precT) = precT

type instance PrimFnShape (SimonsFindXorPeriod sizeT precT) = FindXorPeriodArg

instance P.MapSize (SimonsFindXorPeriod size prec) where
  type MappedSize (SimonsFindXorPeriod size prec) size' = (SimonsFindXorPeriod size' prec)
  mapSize f (SimonsFindXorPeriod p) = SimonsFindXorPeriod (P.mapSize f p)

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance FindXorPeriod sizeT precT :<: SimonsFindXorPeriod sizeT precT

instance IsA (FindXorPeriod sizeT precT) (SimonsFindXorPeriod sizeT precT)

instance (Show sizeT) => SerializePrim (SimonsFindXorPeriod sizeT Double) where
  primNames = ["findXorPeriod"]
  parsePrimParams tp s = SimonsFindXorPeriod <$> parsePrimParams tp s
  printPrimParams (SimonsFindXorPeriod prim) = printPrimParams prim

instance
  (P.TypingReqs size, Num prec, Ord prec, Show prec) =>
  TypeCheckPrim (SimonsFindXorPeriod size prec) size
  where
  inferRetTypesPrim (SimonsFindXorPeriod p) = inferRetTypesPrim p

-- ================================================================================
-- Cost Instances
-- ================================================================================

-- | Number of queries as described in Theorem 1.
_SimonsQueries ::
  forall sizeT precT.
  (Floating precT, P.SizeToPrec sizeT precT) =>
  -- | bitsize
  sizeT ->
  -- | p_0: maximum probability of spurious collisions for non-period values.
  precT ->
  -- | maximum allowed failure probability.
  P.FailProb precT ->
  precT
_SimonsQueries n p0 eps = q
 where
  {- Sketch:
    We need to pick @c@ such that @(2 * ((1 + p0)/2)^c)^n <= eps@ (see Theorem 1)
    ==> 2^n / eps <= (2 / (1+p0))^(cn)
    define q := cn (i.e. number of queries)
    ==> n + log_2 (1/eps) <= q * log_2(2 / (1+p0))
  -}

  q_num = P.sizeToPrec n + logBase 2 (1 / P.getFailProb eps)
  q_den = logBase 2 (2 / (1 + p0))
  q = q_num / q_den

instance
  (P.TypingReqs size, Floating prec, Ord prec, Show prec, P.SizeToPrec size prec) =>
  UnitaryCostPrim (SimonsFindXorPeriod size prec) size prec
  where
  unitaryQueryCosts prim eps =
    let FindXorPeriod{n, p_0} = extract prim :: FindXorPeriod size prec
     in FindXorPeriodArg{fun = strongQueries $ _SimonsQueries n p_0 eps}

  unitaryExprCosts _ _ = Alg.zero

-- | Same as unitary compilation.
instance
  (P.TypingReqs size, Floating prec, Ord prec, Show prec, P.SizeToPrec size prec) =>
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
  [P.VarType size] ->
  m (CQPL.ProcDef size)
simonsOneRound arg_tys = do
  (FindXorPeriodArg call_upred) <- view $ to mk_ucall
  (FindXorPeriodArg pred_aux_tys) <- view $ to uproc_aux_types

  proc_name <- lift $ Compiler.newIdent "SimonOneRound_U"

  xs <- lift $ mapM (Compiler.allocAncillaWithPref "x") arg_tys
  ys <- lift $ mapM (Compiler.allocAncillaWithPref "y") arg_tys
  ys' <- lift $ mapM (Compiler.allocAncillaWithPref "yy") arg_tys
  aux <- lift $ mapM Compiler.allocAncilla pred_aux_tys

  let had_xs = CQPL.USeqS [CQPL.UnitaryS [CQPL.Arg x] (CQPL.DistrU $ P.UniformE t) | (x, t) <- zip xs arg_tys]
  let call_g = call_upred (map CQPL.Arg (xs ++ ys ++ aux))
  let copy_out = CQPL.USeqS [CQPL.UnitaryS [CQPL.Arg y, CQPL.Arg y'] (CQPL.BasicGateU CQPL.COPY) | (y, y') <- zip ys ys']

  let body_compute = CQPL.USeqS [had_xs, call_g]
  let uproc_body_stmt =
        CQPL.USeqS
          [ body_compute
          , copy_out
          , CQPL.adjoint body_compute
          ]

  return
    CQPL.ProcDef
      { CQPL.info_comment = ""
      , CQPL.proc_name
      , CQPL.proc_meta_params = []
      , CQPL.proc_param_types = arg_tys ++ arg_tys ++ arg_tys ++ pred_aux_tys
      , CQPL.proc_body =
          CQPL.ProcBodyU $
            CQPL.UProcBody
              { CQPL.uproc_param_names = xs ++ ys ++ ys' ++ aux
              , CQPL.uproc_param_tags =
                  replicate (length arg_tys) CQPL.ParamOut
                    ++ replicate (length arg_tys) CQPL.ParamAux
                    ++ replicate (length arg_tys) CQPL.ParamAux
                    ++ replicate (length pred_aux_tys) CQPL.ParamAux
              , CQPL.uproc_body_stmt
              }
      }

instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  UnitaryCompilePrim (SimonsFindXorPeriod size prec) size prec
  where
  compileUPrim (SimonsFindXorPeriod FindXorPeriod{n, p_0}) eps = do
    rets <- view $ to ret_vars

    -- arguments (types) over which we compute the period
    arg_tys <- forM rets $ \x -> do
      mty <- use $ P._typingCtx . Ctx.at x
      maybeWithError "" mty

    simons_uproc <- simonsOneRound arg_tys
    Compiler.addProc simons_uproc

    proc_name <- lift $ Compiler.newIdent "USimon"
    i <- lift $ Compiler.newIdent "i"

    let nq = ceiling $ _SimonsQueries n p_0 eps
    xts <- forM (simons_uproc & CQPL.proc_param_types) $ \t -> do
      let t' = P.Arr nq t
      x <- lift $ Compiler.allocAncillaWithPref (proc_name ++ "_aux") t'
      pure (x, t')

    let uproc_body_stmt =
          CQPL.USeqS
            [ CQPL.UForInRangeS
                { CQPL.iter_meta_var = i
                , CQPL.iter_lim = P.MetaSize nq
                , CQPL.uloop_body =
                    CQPL.UCallS
                      { uproc_id = CQPL.proc_name simons_uproc
                      , dagger = False
                      , qargs = map (\(x, _) -> CQPL.ArrElemArg (CQPL.Arg x) (P.MetaName i)) xts
                      }
                , dagger = False
                }
            , CQPL.UCommentS $
                printf
                  "simon's post-processing: unitarily solve linear system: (%s) . (%s) = 0"
                  (PP.commaList rets)
                  (PP.commaList $ take (length arg_tys) $ map fst xts)
            ]

    return
      CQPL.ProcDef
        { CQPL.info_comment = ""
        , CQPL.proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = arg_tys ++ map snd xts
        , CQPL.proc_body =
            CQPL.ProcBodyU $
              CQPL.UProcBody
                { CQPL.uproc_param_names = rets ++ map fst xts
                , CQPL.uproc_param_tags = replicate (length rets) CQPL.ParamOut ++ replicate (length xts) CQPL.ParamAux
                , CQPL.uproc_body_stmt
                }
        }

instance
  (size ~ SizeT, RealFloat prec, Show prec) =>
  QuantumCompilePrim (SimonsFindXorPeriod size prec) size prec
  where
  compileQPrim (SimonsFindXorPeriod FindXorPeriod{n, p_0}) eps = do
    rets <- view $ to ret_vars

    -- arguments (types) over which we compute the period
    arg_tys <- forM rets $ \x -> do
      mty <- use $ P._typingCtx . Ctx.at x
      maybeWithError "" mty

    simons_uproc <- simonsOneRound arg_tys
    Compiler.addProc simons_uproc

    proc_name <- lift $ Compiler.newIdent "QSimon"
    i <- lift $ Compiler.newIdent "i"

    let nq = ceiling $ _SimonsQueries n p_0 eps
    xts <- forM arg_tys $ \t -> do
      let t' = P.Arr nq t
      x <- lift $ Compiler.allocAncillaWithPref (proc_name ++ "__u") t'
      pure (x, t')

    let cproc_body_stmt =
          CQPL.SeqS
            [ CQPL.ForInRangeS
                { CQPL.iter_meta_var = i
                , CQPL.iter_lim = P.MetaSize nq
                , CQPL.loop_body =
                    CQPL.CallS
                      { fun = CQPL.UProcAndMeas (CQPL.proc_name simons_uproc)
                      , meta_params = []
                      , args = map (\(x, _) -> CQPL.ArrElemArg (CQPL.Arg x) (P.MetaName i)) xts
                      }
                }
            , CQPL.CommentS $
                printf
                  "simon's post-processing: solve linear system: (%s) . (%s) = 0"
                  (PP.commaList rets)
                  (PP.commaList $ map fst xts)
            ]

    return
      CQPL.ProcDef
        { CQPL.info_comment = ""
        , CQPL.proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = arg_tys
        , CQPL.proc_body =
            CQPL.ProcBodyC $
              CQPL.CProcBody
                { CQPL.cproc_param_names = rets
                , CQPL.cproc_local_vars = xts
                , CQPL.cproc_body_stmt
                }
        }

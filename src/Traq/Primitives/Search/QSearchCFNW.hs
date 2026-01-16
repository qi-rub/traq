{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
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

  -- * Cost Formulas
  _EQSearch,
  _EQSearchWorst,
  _QSearchZalka,
) where

import Control.Monad (forM, replicateM, when)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), censor, execWriterT, listen)
import Data.Maybe (catMaybes, fromJust)
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
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude
import Traq.ProtoLang (notE, (.&&.), (.+.), (.<=.))
import qualified Traq.ProtoLang as P

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- https://arxiv.org/pdf/2203.04975v2, Eq. 4
_EQSearchWorst :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> A.FailProb precT -> precT
_EQSearchWorst n eps = 9.2 * log (1 / A.getFailProb eps) * sqrt (fromIntegral n)

-- https://arxiv.org/pdf/2203.04975v2, Eq. 3
_F :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> sizeT -> precT
_F n t
  | 4 * t >= n = 2.0344
  | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

-- https://arxiv.org/pdf/2203.04975v2, Eq. 2
_EQSearch :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> sizeT -> A.FailProb precT -> precT
_EQSearch n t eps
  | t == 0 = _EQSearchWorst n eps
  | otherwise = _F n t * (1 + 1 / (1 - term))
 where
  term = _F n t / (9.2 * sqrt (fromIntegral n))

_QSearchZalka :: forall sizeT precT. (Integral sizeT, Floating precT) => sizeT -> A.FailProb precT -> precT
_QSearchZalka n eps = nq_simple
 where
  -- Section 2.1 simple algorithm cost
  nq_simple :: precT
  nq_simple = fromIntegral (_QSearchZalka_max_iter n) * _QSearchZalka_n_reps eps

-- Section 2.2 Improved Algorithm (i.e. sqrt log(1/eps) instead of log(1/eps))
-- log_fac = ceiling log_fac
-- log_fac :: precT
-- log_fac = log (1 / eps) / (2 * log (4 / 3))

-- number of queries of the original algorithm.
-- nq :: precT
-- nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

-- Section 2.1 simple algorithm cost
_QSearchZalka_max_iter :: forall size. (Integral size) => size -> size
_QSearchZalka_max_iter n = ceiling $ (pi / 4) * sqrt (fromIntegral n :: Double)

-- Section 2.1 simple algorithm cost
_QSearchZalka_n_reps :: forall prec. (Floating prec) => A.FailProb prec -> prec
_QSearchZalka_n_reps eps = logBase (1 - p) (A.getFailProb eps)
 where
  p = 0.3914 :: prec

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

newtype QSearchCFNW sizeT precT = QSearchCFNW (PrimSearch sizeT precT)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (QSearchCFNW sizeT precT) = sizeT
type instance PrecType (QSearchCFNW sizeT precT) = precT

type instance PrimFnShape (QSearchCFNW size prec) = BooleanPredicate

instance P.MapSize (QSearchCFNW size prec) where
  type MappedSize (QSearchCFNW size prec) size' = QSearchCFNW size' prec

  mapSize f (QSearchCFNW p) = QSearchCFNW (P.mapSize f p)

instance PrimSearch sizeT precT :<: QSearchCFNW sizeT precT where
  inject = QSearchCFNW
  project (QSearchCFNW p) = Just p

instance (Show sizeT) => SerializePrim (QSearchCFNW sizeT precT) where
  primNames = ["any", "search"]
  primNameOf (QSearchCFNW p) = primNameOf p
  parsePrimParams tp s = QSearchCFNW <$> parsePrimParams tp s
  printPrimParams (QSearchCFNW prim) = printPrimParams prim

-- Type check
instance (P.TypingReqs size) => TypeCheckPrim (QSearchCFNW size prec) size where
  inferRetTypesPrim (QSearchCFNW prim) = inferRetTypesPrim prim

-- Eval
instance EvalPrim (QSearchCFNW size prec) size prec where
  evalPrim (QSearchCFNW prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

getSearchType :: QSearchCFNW size prec -> P.VarType size
getSearchType (QSearchCFNW (PrimSearch{search_ty})) = search_ty

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  (P.TypingReqs sizeT, Integral sizeT, Floating precT) =>
  UnitaryCostPrim (QSearchCFNW sizeT precT) sizeT precT
  where
  unitaryQueryCosts prim eps = BooleanPredicate $ strongQueries $ _QSearchZalka _N eps
   where
    _N = P.domainSize $ getSearchType prim

  unitaryExprCosts _ _ = Alg.zero

instance
  (P.TypingReqs sizeT, Integral sizeT, A.SizeToPrec sizeT precT, Floating precT) =>
  QuantumHavocCostPrim (QSearchCFNW sizeT precT) sizeT precT
  where
  quantumQueryCostsUnitary prim eps = BooleanPredicate $ strongQueries $ _EQSearchWorst _N eps
   where
    _N = P.domainSize $ getSearchType prim

  quantumQueryCostsQuantum _ _ = BooleanPredicate 0

  quantumExprCosts = Alg.zero

instance (sizeT ~ SizeT, Floating precT, Prob.RVType precT precT) => QuantumExpCostPrim (QSearchCFNW sizeT precT) sizeT precT where
  quantumExpQueryCostsUnitary prim eps (BooleanPredicate eval_pred) =
    BooleanPredicate $ strongQueries $ _EQSearch _N _K eps
   where
    search_ty = getSearchType prim
    _N = P.domainSize search_ty

    flags =
      P.domain search_ty <&> \v -> do
        [b] <- Prob.toDeterministicValue $ eval_pred [v]
        pure $ P.valueToBool b

    _K = length $ filter fromJust flags

  quantumExpQueryCostsQuantum _ _ _ = BooleanPredicate []

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Unitary Lowering
-- ================================================================================

-- | Information for building QSearch_Zalka
data UQSearchEnv sizeT = UQSearchEnv
  { search_arg_type :: P.VarType sizeT
  , pred_call_builder :: Ident -> Ident -> Ident -> CQPL.UStmt sizeT
  }

-- | A layer on top of the unitary compiler, holding the relevant QSearch context, and storing the produced statements.
type UQSearchBuilder ext =
  RWST
    (UQSearchEnv (SizeType ext))
    [CQPL.UStmt (SizeType ext)]
    ()
    (CompileU.CompilerT ext)

allocSearchArgReg :: UQSearchBuilder ext Ident
allocSearchArgReg = do
  ty <- view $ to search_arg_type
  lift $ CompileU.allocAncillaWithPref "s_arg" ty

addPredCall :: Ident -> Ident -> Ident -> UQSearchBuilder ext ()
addPredCall c x b = do
  mk_pred <- view $ to pred_call_builder
  writeElem $ mk_pred c x b

withComputed :: (sizeT ~ SizeType ext) => CQPL.UStmt sizeT -> UQSearchBuilder ext a -> UQSearchBuilder ext a
withComputed s m = do
  writeElem s
  a <- m
  writeElem $ CQPL.adjoint s
  return a

addGroverIteration ::
  forall ext sizeT precT.
  ( Integral sizeT
  , RealFloat precT
  , P.TypingReqs sizeT
  , sizeT ~ SizeType ext
  , precT ~ PrecType ext
  ) =>
  -- | ctrl
  Ident ->
  -- | x
  Ident ->
  -- | b
  Ident ->
  UQSearchBuilder ext ()
addGroverIteration c x b = do
  addPredCall c x b
  writeElem $ CQPL.UnitaryS [x] (CQPL.Adjoint CQPL.Unif)
  writeElem $ CQPL.UnitaryS [x] CQPL.Refl0
  writeElem $ CQPL.UnitaryS [x] CQPL.Unif

algoQSearchZalkaRandomIterStep ::
  forall ext sizeT precT.
  ( Integral sizeT
  , RealFloat precT
  , P.TypingReqs sizeT
  , sizeT ~ SizeType ext
  , precT ~ PrecType ext
  ) =>
  -- | max num of iteration
  sizeT ->
  UQSearchBuilder ext Ident
algoQSearchZalkaRandomIterStep r = do
  -- time register
  let r_ty = P.Fin r
  r_reg <- lift $ CompileU.allocAncillaWithPref "n_iter" r_ty
  ctrl_bit <- lift $ CompileU.allocAncillaWithPref "ctrl" P.tbool
  x_reg <- allocSearchArgReg
  b_reg <- lift $ CompileU.allocAncillaWithPref "pred_out" P.tbool

  -- uniform r
  let prep_r = CQPL.UnitaryS [r_reg] CQPL.Unif

  withComputed prep_r $ do
    -- b in minus state for grover
    let prep_b =
          CQPL.USeqS
            [ CQPL.UnitaryS [b_reg] CQPL.XGate
            , CQPL.UnitaryS [b_reg] CQPL.HGate
            ]
    withComputed prep_b $ do
      -- uniform x
      writeElem $ CQPL.UnitaryS [x_reg] CQPL.Unif

      -- controlled iterate
      let meta_ix_name = "LIM"
      let calc_ctrl =
            CQPL.UnitaryS [r_reg, ctrl_bit] $ CQPL.RevEmbedU ["a"] $ "a" .<=. "#LIM"
      ((), grover_body) <-
        censor (const mempty) $
          listen $
            withComputed calc_ctrl $ do
              addGroverIteration ctrl_bit x_reg b_reg
      writeElem $ CQPL.mkForInRangeS meta_ix_name (P.MetaSize r) (CQPL.USeqS grover_body)

  withComputed (CQPL.UnitaryS [ctrl_bit] CQPL.XGate) $
    addPredCall ctrl_bit x_reg b_reg
  return b_reg

algoQSearchZalka ::
  forall ext sizeT precT.
  ( Integral sizeT
  , RealFloat precT
  , P.TypingReqs sizeT
  , sizeT ~ SizeType ext
  , precT ~ PrecType ext
  ) =>
  -- | max. error (TV/trace-norm)
  A.FailProb precT ->
  -- | output bit
  Ident ->
  UQSearchBuilder ext ()
algoQSearchZalka eps out_bit = do
  n <- view $ to search_arg_type . singular P._Fin

  out_bits <- forM [1 :: Int .. (ceiling (_QSearchZalka_n_reps eps))] $ \i -> do
    writeElem $ CQPL.UCommentS " "
    writeElem $ CQPL.UCommentS $ printf "Run %d" i
    writeElem $ CQPL.UCommentS " "
    algoQSearchZalkaRandomIterStep (_QSearchZalka_max_iter n)

  let as = ["a" <> show i | i <- [1 .. length out_bits]]
  writeElem $
    CQPL.UnitaryS
      { CQPL.qargs = out_bits ++ [out_bit]
      , CQPL.unitary = CQPL.RevEmbedU as $ P.NAryE P.MultiOrOp (map fromString as)
      }

instance
  ( Integral sizeT
  , RealFloat precT
  , Show sizeT
  , Show precT
  , P.TypingReqs sizeT
  , A.SizeToPrec sizeT precT
  ) =>
  CompileU.Lowerable (A.AnnFailProb (Primitive (QSearchCFNW sizeT precT))) sizeT precT
  where
  lowerPrimitive (A.AnnFailProb eps (Primitive [PartialFun{pfun_name, pfun_args}] (QSearchCFNW PrimSearch{}))) [ret] = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (P._funCtx . Ctx.at pfun_name)
        >>= maybeWithError ("cannot find predicate " <> pfun_name)

    -- size of the search space
    let s_ty = last param_types
    let n = s_ty ^?! P._Fin

    -- compile the predicate
    CompileU.LoweredProc
      { CompileU.lowered_def = pred_proc
      , CompileU.has_ctrl = _
      , CompileU.inp_tys = pred_inp_tys
      , CompileU.out_tys = pred_out_tys
      , CompileU.aux_tys = pred_aux_tys
      } <-
      CompileU.lowerFunDef CompileU.WithControl pfun_name pred_fun

    when (pred_out_tys /= [P.tbool]) $ throwError "invalid outputs for predicate"
    when (last pred_inp_tys /= s_ty) $ throwError "mismatched search argument type"

    -- function to call the predicate, re-using the same aux space each time.
    pred_ancilla <- mapM CompileU.allocAncilla pred_aux_tys
    let pred_caller ctrl x b =
          CQPL.UCallS
            { CQPL.uproc_id = CQPL.proc_name pred_proc
            , CQPL.dagger = False
            , CQPL.qargs = ctrl : placeArgs pfun_args [x] ++ [b] ++ pred_ancilla
            }

    -- Emit the qsearch procedure
    -- body:
    (qsearch_body, qsearch_ancilla) <- do
      ini_binds <- use P._typingCtx
      ((), ss) <- (\m -> evalRWST m UQSearchEnv{search_arg_type = s_ty, pred_call_builder = pred_caller} ()) $ algoQSearchZalka eps ret
      fin_binds <- use P._typingCtx
      let ancillas = Ctx.toList $ fin_binds Ctx.\\ ini_binds
      return (CQPL.USeqS ss, ancillas)

    -- name:
    -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
    qsearch_proc_name <- Compiler.newIdent "UAny"
    let info_comment =
          (printf :: String -> String -> String -> String -> String)
            "QSearch[%s, %s, %s]"
            (show n)
            (show $ A.getFailProb eps)
            (CQPL.proc_name pred_proc)
    let all_params =
          CompileU.withTag CQPL.ParamInp (zip (catMaybes pfun_args) (init pred_inp_tys))
            ++ CompileU.withTag CQPL.ParamOut [(ret, P.tbool)]
            ++ CompileU.withTag CQPL.ParamAux (zip pred_ancilla pred_aux_tys)
            ++ CompileU.withTag CQPL.ParamAux qsearch_ancilla

    -- add the proc:
    Compiler.addProc
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

    return
      CQPL.UCallS
        { CQPL.uproc_id = qsearch_proc_name
        , CQPL.qargs = catMaybes pfun_args ++ [ret] ++ pred_ancilla ++ map fst qsearch_ancilla
        , CQPL.dagger = False
        }

  -- fallback
  lowerPrimitive _ _ = throwError "Unsupported"

-- ================================================================================
-- CQ Lowering
-- ================================================================================

-- | the generated QSearch procedure: body stmts and local vars
type QSearchCompilerT ext =
  WriterT
    ([CQPL.Stmt (SizeType ext)], [(Ident, P.VarType (SizeType ext))])
    (CompileQ.CompilerT ext)

allocReg ::
  ( m ~ QSearchCompilerT ext
  , sizeT ~ SizeType ext
  ) =>
  Ident ->
  P.VarType sizeT ->
  m Ident
allocReg prefix ty = do
  reg <- lift $ Compiler.newIdent prefix
  writeElemAt _2 (reg, ty)
  return reg

-- | Run K grover iterations
groverK ::
  forall sizeT.
  -- | number of rounds
  P.MetaParam sizeT ->
  -- | the element and type to search for. @x : T@
  (Ident, P.VarType sizeT) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> CQPL.UStmt sizeT) ->
  CQPL.UStmt sizeT
groverK k (x, _) b mk_pred =
  CQPL.USeqS
    [ prepb
    , prepx
    , CQPL.URepeatS k grover_iterate
    , CQPL.adjoint prepb
    ]
 where
  -- map b to |-> and x to uniform
  prepb, prepx :: CQPL.UStmt sizeT
  prepb =
    CQPL.USeqS
      [ CQPL.UnitaryS [b] CQPL.XGate
      , CQPL.UnitaryS [b] CQPL.HGate
      ]
  prepx = CQPL.UnitaryS [x] CQPL.Unif

  grover_iterate :: CQPL.UStmt sizeT
  grover_iterate =
    CQPL.USeqS
      [ mk_pred x b
      , CQPL.UnitaryS [x] (CQPL.Adjoint CQPL.Unif)
      , CQPL.UnitaryS [x] CQPL.Refl0
      , CQPL.UnitaryS [x] CQPL.Unif
      ]

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
algoQSearch ::
  forall ext sizeT precT.
  ( Integral sizeT
  , RealFloat precT
  , sizeT ~ SizeT
  , CompileQ.Lowerable ext sizeT precT
  , Show sizeT
  , Show precT
  , P.TypingReqs sizeT
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
  -- | Result register
  Ident ->
  -- | the generated QSearch procedure: body stmts and local vars
  WriterT ([CQPL.Stmt sizeT], [(Ident, P.VarType sizeT)]) (CompileQ.CompilerT ext) ()
algoQSearch ty n_samples eps grover_k_caller pred_caller ok = do
  not_done <- allocReg "not_done" P.tbool
  q_sum <- allocReg "Q_sum" j_type
  j <- allocReg "j" j_type
  j_lim <- allocReg "j_lim" j_type
  x <- allocReg "x" ty

  -- classical sampling
  when (n_samples /= 0) $ do
    let classicalSampling =
          CQPL.WhileKWithCondExpr (CQPL.MetaSize n_samples) not_done (notE (fromString ok)) $
            CQPL.SeqS
              [ CQPL.RandomS x (P.MetaSize n)
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
  n = ty ^?! P._Fin

  alpha = 9.2
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

instance
  ( Integral sizeT
  , RealFloat precT
  , sizeT ~ SizeT
  , Show sizeT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  CompileQ.Lowerable (A.AnnFailProb (Primitive (QSearchCFNW sizeT precT))) sizeT precT
  where
  lowerPrimitive (A.AnnFailProb eps (Primitive [PartialFun{pfun_name, pfun_args}] (QSearchCFNW (PrimSearch _ s_ty)))) (ret : rets) = do
    -- predicate, pred_args = args
    -- the predicate
    pred_fun <-
      view (P._funCtx . Ctx.at pfun_name)
        >>= maybeWithError ("cannot find predicate " <> pfun_name)

    -- lower the unitary predicate
    pred_uproc <- CompileU.lowerFunDef @_ CompileU.WithoutControl pfun_name pred_fun

    let CompileU.LoweredProc
          { CompileU.inp_tys = pred_inp_tys
          , CompileU.aux_tys = pred_aux_tys
          -- , CQPL.out_tys = pred_out_tys
          } = pred_uproc
    let upred_proc_name = pred_uproc ^. to CompileU.lowered_def . to CQPL.proc_name

    -- make the Grover_k uproc
    -- TODO this should ideally be done by algoQSearch, but requires a lot of aux information.
    uproc_grover_k_name <- Compiler.newIdent "Grover"
    upred_aux_vars <- replicateM (length pred_aux_tys) $ Compiler.newIdent "aux"
    grover_arg_name <- Compiler.newIdent "x"
    let meta_k = P.MetaName "k"
    let uproc_grover_k_body =
          groverK
            meta_k
            (grover_arg_name, s_ty)
            ret
            ( \x b ->
                CQPL.UCallS
                  { CQPL.uproc_id = upred_proc_name
                  , CQPL.dagger = False
                  , CQPL.qargs = catMaybes pfun_args ++ [x, b] ++ upred_aux_vars
                  }
            )
    let uproc_grover_k_params =
          CompileU.withTag CQPL.ParamInp (zip (catMaybes pfun_args ++ [grover_arg_name]) pred_inp_tys)
            ++ CompileU.withTag CQPL.ParamOut [(ret, P.tbool)]
            ++ CompileU.withTag CQPL.ParamAux (zip upred_aux_vars pred_aux_tys)
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
            , CQPL.args = [x, b]
            }

    -- emit the QSearch algorithm
    qsearch_params <- forM (catMaybes pfun_args ++ [ret]) $ \x -> do
      ty <- use $ P._typingCtx . Ctx.at x . singular _Just
      return (x, ty)

    let pred_caller x b =
          CQPL.CallS
            { CQPL.fun = CQPL.UProcAndMeas upred_proc_name
            , CQPL.meta_params = []
            , CQPL.args = catMaybes pfun_args ++ [x, b]
            }

    (qsearch_body, qsearch_local_vars) <- execWriterT $ algoQSearch s_ty 0 eps grover_k_caller pred_caller ret
    qsearch_proc_name <- Compiler.newIdent "QAny"
    Compiler.addProc $
      CQPL.ProcDef
        { CQPL.info_comment = printf "QAny[%s]" (show $ A.getFailProb eps)
        , CQPL.proc_name = qsearch_proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = map snd qsearch_params
        , CQPL.proc_body =
            CQPL.ProcBodyC $
              CQPL.CProcBody
                { CQPL.cproc_param_names = map fst qsearch_params
                , CQPL.cproc_local_vars = qsearch_local_vars
                , CQPL.cproc_body_stmt = CQPL.SeqS qsearch_body
                }
        }

    return
      CQPL.CallS
        { CQPL.fun = CQPL.FunctionCall qsearch_proc_name
        , CQPL.args = catMaybes pfun_args ++ [ret] ++ rets
        , CQPL.meta_params = []
        }
  lowerPrimitive _ _ = error "Unsupported"

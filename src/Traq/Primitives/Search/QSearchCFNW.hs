{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Control.Applicative ((<|>))
import Control.Monad (forM, replicateM, when)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT (..), censor, execWriterT, listen)
import Data.String (fromString)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Parsec (try)
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Search.Prelude
import Traq.ProtoLang (notE, (.&&.), (.+.), (.<=.))
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- Eq. TODO REF
_EQSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_EQSearchWorst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

-- Eq. TODO REF
_F :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT
_F n t
  | 4 * t >= n = 2.0344
  | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

-- Eq. TODO REF
_EQSearch :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT -> costT
_EQSearch n t eps
  | t == 0 = _EQSearchWorst n eps
  | otherwise = _F n t * (1 + 1 / (1 - term))
 where
  term = _F n t / (9.2 * sqrt (fromIntegral n))

-- Eq. TODO REF
_QSearchZalka :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_QSearchZalka n delta = 2 * nq_simple -- 2x for compute-uncompute
 where
  -- Section 2.2 Improved Algorithm (i.e. sqrt log(1/eps) instead of log(1/eps))
  -- fail prob
  eps :: costT
  eps = (delta / 2) ^ (2 :: Int)

  -- log_fac = ceiling log_fac
  -- log_fac :: costT
  -- log_fac = log (1 / eps) / (2 * log (4 / 3))

  -- number of queries of the original algorithm.
  -- nq :: costT
  -- nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

  -- Section 2.1 simple algorithm cost
  max_iter :: sizeT
  max_iter = ceiling $ (pi / 4) * sqrt (fromIntegral n :: Double)

  n_reps :: costT
  n_reps = logBase (1 - p) eps
   where
    p = 0.3914 :: costT

  nq_simple :: costT
  nq_simple = fromIntegral max_iter * n_reps

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QSearchCFNW = QSearchCFNW {predicate :: Ident, return_sol :: Bool}
  deriving (Eq, Show, Read)

instance HasPrimAny QSearchCFNW where
  mkAny p = QSearchCFNW{predicate = p, return_sol = False}
  getPredicateOfAny = predicate

instance HasPrimSearch QSearchCFNW where
  mkSearch p = QSearchCFNW{predicate = p, return_sol = True}
  getPredicateOfSearch = predicate

instance PP.ToCodeString QSearchCFNW where
  build QSearchCFNW{predicate, return_sol = False} = PP.putWord $ printf "@any[%s]" predicate
  build QSearchCFNW{predicate, return_sol = True} = PP.putWord $ printf "@search[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QSearchCFNW where
  primitiveParser tp = try parseAny <|> try parseSearch
   where
    parseAny = do
      [predicate] <- parsePrimWithPredicates "any" 1 tp
      return QSearchCFNW{predicate, return_sol = False}
    parseSearch = do
      [predicate] <- parsePrimWithPredicates "search" 1 tp
      return QSearchCFNW{predicate, return_sol = True}

-- Type check
instance P.TypeCheckablePrimitive QSearchCFNW sizeT where
  typeCheckPrimitive prim@QSearchCFNW{return_sol = False} = typeCheckPrimAny prim
  typeCheckPrimitive prim@QSearchCFNW{return_sol = True} = typeCheckPrimSearch prim

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance
  ( Fractional costT
  , P.EvaluatablePrimitive primsT primsT costT
  ) =>
  P.EvaluatablePrimitive primsT QSearchCFNW costT
  where
  evalPrimitive prim@QSearchCFNW{return_sol = False} = evaluatePrimAny prim
  evalPrimitive prim@QSearchCFNW{return_sol = True} = evaluatePrimSearch prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  unitaryQueryCostPrimitive delta QSearchCFNW{predicate} _ = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    let n = last param_types ^?! P._Fin

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search

    -- number of predicate queries
    let qry = _QSearchZalka n delta_search

    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- cost of each predicate call
    cost_pred <-
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_pred

instance
  ( Integral sizeT
  , Floating costT
  , P.QuantumMaxCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.QuantumMaxCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  quantumMaxQueryCostPrimitive eps QSearchCFNW{predicate} = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    let n = last param_types ^?! P._Fin

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of predicate queries
    let qry = _EQSearchWorst n eps_search

    -- fail prob per predicate call
    let eps_per_pred_call = eps_pred / qry
    let delta_per_pred_call = eps_per_pred_call / 2

    -- cost of each predicate call
    cost_unitary_pred <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call $
          P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

instance
  ( Integral sizeT
  , Floating costT
  , Show costT
  , P.EvaluatablePrimitive primsT QSearchCFNW costT
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  quantumQueryCostPrimitive eps QSearchCFNW{predicate} _ = do
    P.FunDef{P.param_types} <- view $ P._funCtx . Ctx.at predicate . singular _Just
    let typ_x = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of solutions
    let space = P.domain typ_x
    let n = length space
    t <- do
      env <- view P._evaluationEnv
      runSearchPredicateOnAllInputs @primsT @costT predicate space
        & (runReaderT ?? env)
        & Prob.toDeterministicValue
        & fmap countSolutions
        & lift

    -- number of predicate queries
    let qry = _EQSearch n t eps_search

    let q_worst = _EQSearchWorst n eps_search
    let eps_per_pred_call = eps_pred / q_worst
    let delta_per_pred_call = eps_per_pred_call / 2

    pred_unitary_cost <-
      magnify P._unitaryCostEnv $
        P.unitaryQueryCostE delta_per_pred_call P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}
    return $ qry * pred_unitary_cost

-- ================================================================================
-- Unitary Lowering
-- ================================================================================

-- | Temporary black-boxes for un-implemented parts of the quantum search algorithms.
data QSearchBlackBoxes costT
  = QSearchBlackBox {q_pred_name :: Ident, n_pred_calls :: costT}
  | TODOHole String
  deriving (Eq, Show)

instance CQPL.HoleCost (QSearchBlackBoxes costT) costT where
  holeCost QSearchBlackBox{q_pred_name, n_pred_calls} = do
    pred_cost <- CQPL.procCost q_pred_name
    return $ n_pred_calls * pred_cost
  holeCost (TODOHole s) = error $ "no cost of unknown hole: " <> s

-- | Information for building QSearch_Zalka
data UQSearchEnv holeT sizeT = UQSearchEnv
  { search_arg_type :: P.VarType sizeT
  , pred_call_builder :: Ident -> Ident -> Ident -> CQPL.UStmt holeT sizeT
  }

-- | A layer on top of the unitary compiler, holding the relevant QSearch context, and storing the produced statements.
type UQSearchBuilder primsT holeT sizeT costT =
  RWST
    (UQSearchEnv holeT sizeT)
    [CQPL.UStmt holeT sizeT]
    ()
    (CompileU.CompilerT primsT holeT sizeT costT)

allocSearchArgReg :: UQSearchBuilder primsT holeT sizeT costT Ident
allocSearchArgReg = do
  ty <- view $ to search_arg_type
  lift $ CompileU.allocAncillaWithPref "s_arg" ty

addPredCall :: Ident -> Ident -> Ident -> UQSearchBuilder primsT holeT sizeT costT ()
addPredCall c x b = do
  mk_pred <- view $ to pred_call_builder
  writeElem $ mk_pred c x b

withComputed :: CQPL.UStmt holeT sizeT -> UQSearchBuilder primsT holeT sizeT costT a -> UQSearchBuilder primsT holeT sizeT costT a
withComputed s m = do
  writeElem s
  a <- m
  writeElem $ CQPL.adjoint s
  return a

addGroverIteration ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , P.TypeCheckable sizeT
  ) =>
  -- | ctrl
  Ident ->
  -- | x
  Ident ->
  -- | b
  Ident ->
  UQSearchBuilder primsT holeT sizeT costT ()
addGroverIteration c x b = do
  addPredCall c x b
  writeElem $ CQPL.UnitaryS [x] (CQPL.Adjoint CQPL.Unif)
  writeElem $ CQPL.UnitaryS [x] CQPL.Refl0
  writeElem $ CQPL.UnitaryS [x] CQPL.Unif

algoQSearchZalkaRandomIterStep ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , P.TypeCheckable sizeT
  ) =>
  -- | max num of iteration
  sizeT ->
  UQSearchBuilder primsT holeT sizeT costT Ident
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
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , P.TypeCheckable sizeT
  ) =>
  -- | max. norm error @\delta@
  costT ->
  -- | output bit
  Ident ->
  UQSearchBuilder primsT holeT sizeT costT ()
algoQSearchZalka delta out_bit = do
  n <- view $ to search_arg_type . singular P._Fin

  out_bits <- forM [1 .. n_reps] $ \i -> do
    writeElem $ CQPL.UCommentS " "
    writeElem $ CQPL.UCommentS $ printf "Run %d" i
    writeElem $ CQPL.UCommentS " "
    algoQSearchZalkaRandomIterStep (max_iter n)

  let as = ["a" <> show i | i <- [1 .. length out_bits]]
  writeElem $
    CQPL.UnitaryS
      { CQPL.qargs = out_bits ++ [out_bit]
      , CQPL.unitary = CQPL.RevEmbedU as $ P.NAryE P.MultiOrOp (map fromString as)
      }
 where
  max_iter :: sizeT -> sizeT
  max_iter n = ceiling $ (pi / 4) * sqrt (fromIntegral n :: Double)

  eps :: costT
  eps = (delta / 2) ^ (2 :: Int)

  n_reps :: Int
  n_reps = ceiling $ logBase (1 - p) eps
   where
    p = 0.3914 :: costT

shouldUncomputeQSearch :: Bool
shouldUncomputeQSearch = False

instance
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , CompileU.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  CompileU.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive delta QSearchCFNW{predicate, return_sol = False} args [ret] = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (P._funCtx . Ctx.at predicate)
        >>= maybeWithError ("cannot find predicate " <> predicate)

    -- size of the search space
    let s_ty = last param_types
    let n = s_ty ^?! P._Fin

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search
    -- number of predicate queries
    let qry = _QSearchZalka n delta_search
    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- compile the predicate
    CompileU.LoweredProc
      { CompileU.lowered_def = pred_proc
      , CompileU.has_ctrl = _
      , CompileU.inp_tys = pred_inp_tys
      , CompileU.out_tys = pred_out_tys
      , CompileU.aux_tys = pred_aux_tys
      } <-
      CompileU.lowerFunDef CompileU.WithControl delta_per_pred_call predicate pred_fun

    when (pred_out_tys /= [P.tbool]) $ throwError "invalid outputs for predicate"
    when (last pred_inp_tys /= s_ty) $ throwError "mismatched search argument type"

    -- function to call the predicate, re-using the same aux space each time.
    pred_ancilla <- mapM CompileU.allocAncilla pred_aux_tys
    let pred_caller ctrl x b =
          CQPL.UCallS
            { CQPL.uproc_id = CQPL.proc_name pred_proc
            , CQPL.dagger = False
            , CQPL.qargs = ctrl : args ++ [x, b] ++ pred_ancilla
            }

    -- Emit the qsearch procedure
    -- body:
    (qsearch_body, qsearch_ancilla) <- do
      ini_binds <- use P._typingCtx
      ((), ss) <- (\m -> evalRWST m UQSearchEnv{search_arg_type = s_ty, pred_call_builder = pred_caller} ()) $ algoQSearchZalka delta_search ret
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
            (show delta_search)
            (CQPL.proc_name pred_proc)
    let all_params =
          CompileU.withTag CQPL.ParamInp (zip args (init pred_inp_tys))
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

    if not shouldUncomputeQSearch
      then
        return
          CQPL.UCallS
            { CQPL.uproc_id = qsearch_proc_name
            , CQPL.qargs = args ++ [ret] ++ pred_ancilla ++ map fst qsearch_ancilla
            , CQPL.dagger = False
            }
      else do
        -- clean version of qsearch: uncompute to clean up ancilla
        qsearch_clean_proc_name <- Compiler.newIdent "UAny"
        let info_comment_clean =
              (printf :: String -> String -> String -> String -> String)
                "QSearch_clean[%s, %s, %s]"
                (show n)
                (show delta_search)
                (CQPL.proc_name pred_proc)

        out_bit <- CompileU.allocAncilla P.tbool

        let all_params' =
              CompileU.withTag CQPL.ParamInp (zip args (init pred_inp_tys))
                ++ CompileU.withTag CQPL.ParamOut [(ret, P.tbool)]
                ++ CompileU.withTag CQPL.ParamAux (zip pred_ancilla pred_aux_tys)
                ++ CompileU.withTag CQPL.ParamAux qsearch_ancilla
                ++ CompileU.withTag CQPL.ParamAux [(out_bit, P.tbool)]

        Compiler.addProc
          CQPL.ProcDef
            { CQPL.info_comment = info_comment_clean
            , CQPL.proc_name = qsearch_clean_proc_name
            , CQPL.proc_meta_params = []
            , CQPL.proc_param_types = map (view _3) all_params'
            , CQPL.proc_body =
                CQPL.ProcBodyU $
                  CQPL.UProcBody
                    { CQPL.uproc_param_names = map (view _1) all_params'
                    , CQPL.uproc_param_tags = map (view _2) all_params'
                    , CQPL.uproc_body_stmt =
                        CQPL.UWithComputedS
                          ( CQPL.UCallS
                              { CQPL.uproc_id = qsearch_proc_name
                              , CQPL.qargs = args ++ [out_bit] ++ pred_ancilla ++ map fst qsearch_ancilla
                              , CQPL.dagger = False
                              }
                          )
                          (CQPL.UnitaryS [out_bit, ret] (CQPL.RevEmbedU ["a"] "a"))
                    }
            }

        return
          CQPL.UCallS
            { CQPL.uproc_id = qsearch_clean_proc_name
            , CQPL.qargs = args ++ [ret] ++ pred_ancilla ++ map fst qsearch_ancilla ++ [out_bit]
            , CQPL.dagger = False
            }

  -- fallback
  lowerPrimitive _ _ _ _ = throwError "Unsupported"

-- ================================================================================
-- CQ Lowering
-- ================================================================================

-- | the generated QSearch procedure: body stmts and local vars
type QSearchCompilerT primsT holeT sizeT costT =
  WriterT
    ([CQPL.Stmt holeT sizeT], [(Ident, P.VarType sizeT)])
    (CompileQ.CompilerT primsT holeT sizeT costT)

allocReg ::
  (m ~ QSearchCompilerT primsT holeT sizeT costT) =>
  Ident ->
  P.VarType sizeT ->
  m Ident
allocReg prefix ty = do
  reg <- lift $ Compiler.newIdent prefix
  writeElemAt _2 (reg, ty)
  return reg

-- | Run K grover iterations
groverK ::
  forall holeT sizeT.
  -- | number of rounds
  P.MetaParam sizeT ->
  -- | the element and type to search for. @x : T@
  (Ident, P.VarType sizeT) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> CQPL.UStmt holeT sizeT) ->
  CQPL.UStmt holeT sizeT
groverK k (x, _) b mk_pred =
  CQPL.USeqS
    [ prepb
    , prepx
    , CQPL.URepeatS k grover_iterate
    , CQPL.adjoint prepb
    ]
 where
  -- map b to |-> and x to uniform
  prepb, prepx :: CQPL.UStmt holeT sizeT
  prepb =
    CQPL.USeqS
      [ CQPL.UnitaryS [b] CQPL.XGate
      , CQPL.UnitaryS [b] CQPL.HGate
      ]
  prepx = CQPL.UnitaryS [x] CQPL.Unif

  grover_iterate :: CQPL.UStmt holeT sizeT
  grover_iterate =
    CQPL.USeqS
      [ mk_pred x b
      , CQPL.UnitaryS [x] (CQPL.Adjoint CQPL.Unif)
      , CQPL.UnitaryS [x] CQPL.Refl0
      , CQPL.UnitaryS [x] CQPL.Unif
      ]

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
algoQSearch ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , sizeT ~ SizeT
  , holeT ~ QSearchBlackBoxes costT
  , CompileQ.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  -- | search elem type
  P.VarType sizeT ->
  -- | number of classical samples
  sizeT ->
  -- | max fail prob
  costT ->
  -- | grover_k caller: k, x, b
  (Either (CQPL.MetaParam sizeT) Ident -> Ident -> Ident -> CQPL.Stmt holeT sizeT) ->
  -- | cqpl predicate caller
  (Ident -> Ident -> CQPL.Stmt holeT sizeT) ->
  -- | Result register
  Ident ->
  -- | the generated QSearch procedure: body stmts and local vars
  WriterT ([CQPL.Stmt holeT sizeT], [(Ident, P.VarType sizeT)]) (CompileQ.CompilerT primsT holeT sizeT costT) ()
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

instance
  ( Integral sizeT
  , RealFloat costT
  , sizeT ~ SizeT
  , holeT ~ QSearchBlackBoxes costT
  , CompileQ.Lowerable primsT primsT holeT sizeT costT
  , CompileU.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  CompileQ.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive eps QSearchCFNW{predicate, return_sol = False} args [ret] = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (P._funCtx . Ctx.at predicate)
        >>= maybeWithError ("cannot find predicate " <> predicate)

    -- size of the search space
    let s_ty = last param_types
    let n = s_ty ^?! P._Fin

    -- fail prob of search
    let eps_s = eps / 2

    -- fail prob predicate
    let eps_pred = eps - eps_s
    let n_max_pred_calls = _EQSearchWorst n eps_pred
    let eps_per_pred_call = eps_pred / n_max_pred_calls
    let delta_per_pred_call = eps_per_pred_call / 2 -- norm error in unitary predicate

    -- lower the unitary predicate
    pred_uproc <- WriterT . magnify P._unitaryCostEnv . runWriterT $ do
      CompileU.lowerFunDef @_ @holeT CompileU.WithoutControl delta_per_pred_call predicate pred_fun

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
                  , CQPL.qargs = args ++ [x, b] ++ upred_aux_vars
                  }
            )
    let uproc_grover_k_params =
          CompileU.withTag CQPL.ParamInp (zip (args ++ [grover_arg_name]) pred_inp_tys)
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
    qsearch_params <- forM (args ++ [ret]) $ \x -> do
      ty <- use $ P._typingCtx . Ctx.at x . singular _Just
      return (x, ty)

    -- let upred_caller = (\x b -> CQPL.holeS $ TODOHole $ printf "unitary predicate call (%s, %s)" x b)

    let pred_caller x b =
          CQPL.CallS
            { CQPL.fun = CQPL.UProcAndMeas upred_proc_name
            , CQPL.meta_params = []
            , CQPL.args = args ++ [x, b]
            }

    (qsearch_body, qsearch_local_vars) <- execWriterT $ algoQSearch s_ty 0 eps_s grover_k_caller pred_caller ret
    qsearch_proc_name <- Compiler.newIdent "QAny"
    Compiler.addProc $
      CQPL.ProcDef
        { CQPL.info_comment = printf "QAny[%s]" (show eps_s)
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
        , CQPL.args = args ++ [ret]
        , CQPL.meta_params = []
        }
  lowerPrimitive _ _ _ _ = error "Unsupported"

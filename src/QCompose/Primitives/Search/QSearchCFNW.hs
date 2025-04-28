{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Cost formulas and implementations of the quantum search algorithms in Ref [1].

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module QCompose.Primitives.Search.QSearchCFNW (
  -- * Search Primitive
  QSearchCFNW (..),

  -- * Unitary Implementation
  algoQSearchZalka,

  -- * CQ Implementation

  -- * Cost Formulas
  _EQSearch,
  _EQSearchWorst,
  _QSearchZalka,
) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, forM, replicateM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (anyM)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec (try)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import qualified QCompose.CQPL as CQPL
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives.Search.Prelude

-- ================================================================================
-- Cost Formulas
-- ================================================================================

-- Eq. TODO REF
_EQSearchWorst :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> costT -> costT
_EQSearchWorst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

-- Eq. TODO REF
_F :: forall sizeT costT. (Integral sizeT, Floating costT) => sizeT -> sizeT -> costT
_F n t
  | 4 * t < n = 2.0344
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
_QSearchZalka n delta = 2 * nq -- for compute-uncompute
 where
  -- fail prob
  eps :: costT
  eps = (delta / 2) ^ (2 :: Int)

  -- log_fac = ceiling log_fac
  log_fac :: costT
  log_fac = log (1 / eps) / (2 * log (4 / 3))

  -- number of queries of the original algorithm.
  nq :: costT
  nq = 5 * log_fac + pi * sqrt (fromIntegral n * log_fac)

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QSearchCFNW = QSearchCFNW {predicate :: Ident, return_sol :: Bool}
  deriving (Eq, Show, Read)

instance HasSearch QSearchCFNW where
  mkAny p = QSearchCFNW{predicate = p, return_sol = False}
  mkSearch p = QSearchCFNW{predicate = p, return_sol = True}

  getPredicate = predicate

instance ToCodeString QSearchCFNW where
  toCodeString QSearchCFNW{predicate, return_sol = False} = printf "@any[%s]" predicate
  toCodeString QSearchCFNW{predicate, return_sol = True} = printf "@search[%s]" predicate

-- Parsing
instance P.CanParsePrimitive QSearchCFNW where
  primitiveParser tp = try parseAny <|> try parseSearch
   where
    parseAny = do
      symbol tp "@any"
      predicate <- brackets tp $ identifier tp
      return QSearchCFNW{predicate, return_sol = False}
    parseSearch = do
      symbol tp "@search"
      predicate <- brackets tp $ identifier tp
      return QSearchCFNW{predicate, return_sol = True}

-- Type check
instance P.TypeCheckablePrimitive QSearchCFNW sizeT where
  typeCheckPrimitive QSearchCFNW{predicate, return_sol} args = do
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return $ P.tbool : [last param_types | return_sol]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT QSearchCFNW
  where
  evalPrimitive QSearchCFNW{predicate, return_sol = False} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    has_sol <- flip anyM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    return [P.boolToValue has_sol]
  evalPrimitive QSearchCFNW{predicate, return_sol = True} arg_vals = do
    pred_fun <- view $ _1 . Ctx.at predicate . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    let search_range = pred_fun ^. to P.param_types . to last . to P.range

    sols <- flip filterM search_range $ \val -> do
      res <- P.evalFun (arg_vals ++ [val]) pred_fun
      return $ head res /= 0

    let has_sol = not $ null sols
    let out_vals = if has_sol then sols else search_range
    lift $ Tree.choice [pure [P.boolToValue has_sol, v] | v <- out_vals]

-- ================================================================================
-- Abstract Costs
-- ================================================================================

-- | Compute the unitary cost using the QSearch_Zalka cost formula.
instance
  ( Integral sizeT
  , Floating costT
  , P.UnitaryCostablePrimitive primsT primsT sizeT costT
  ) =>
  P.UnitaryCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  unitaryQueryCostPrimitive delta QSearchCFNW{predicate} = do
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

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
    P.FunDef{P.param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let P.Fin n = last param_types

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
      P.unitaryQueryCostE delta_per_pred_call $
        P.FunCallE{P.fun_kind = P.FunctionCall predicate, P.args = undefined}

    return $ qry * cost_unitary_pred

instance
  ( Integral sizeT
  , Floating costT
  , P.EvaluatablePrimitive primsT QSearchCFNW
  , P.QuantumCostablePrimitive primsT primsT sizeT costT
  , sizeT ~ SizeT
  ) =>
  P.QuantumCostablePrimitive primsT QSearchCFNW sizeT costT
  where
  quantumQueryCostPrimitive eps QSearchCFNW{predicate} vs = do
    predDef@P.FunDef{P.param_types} <- view $ _1 . Ctx.at predicate . singular _Just
    let typ_x = last param_types

    -- split the fail prob
    let eps_search = eps / 2
    let eps_pred = eps - eps_search

    -- number of solutions
    let space = P.range typ_x
    sols <- do
      env <- (,) <$> view _1 <*> view _2
      -- TODO this is too convoluted...
      return $
        (`runMyReaderT` env) $
          (`filterM` space)
            ( \v -> do
                result <- P.evalFun (vs ++ [v]) predDef
                let [b] = result
                return $ b /= 0
            )

    let n = length space
    let t = minimum $ fmap length sols

    -- number of predicate queries
    let qry = _EQSearch n t eps_search

    let q_worst = _EQSearchWorst n eps_search
    let eps_per_pred_call = eps_pred / q_worst
    let delta_per_pred_call = eps_per_pred_call / 2

    pred_unitary_cost <-
      magnify P.extractUEnv $
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

instance UQPL.HoleCost (QSearchBlackBoxes costT) costT where
  holeCost QSearchBlackBox{q_pred_name, n_pred_calls} = do
    pred_cost <- UQPL.procCost q_pred_name
    return $ n_pred_calls * pred_cost
  holeCost (TODOHole s) = error $ "no cost of unknown hole: " <> s

-- | Run K grover iterations
groverK ::
  forall holeT sizeT.
  -- | number of rounds
  UQPL.MetaParam sizeT ->
  -- | the element and type to search for. @x : T@
  (Ident, P.VarType sizeT) ->
  -- | the output bit
  Ident ->
  -- | run the predicate
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  UQPL.Stmt holeT sizeT
groverK k (x, ty) b mk_pred =
  UQPL.SeqS
    [ prepb
    , prepx
    , UQPL.RepeatS k grover_iterate
    , UQPL.adjoint prepb
    ]
 where
  -- map b to |-> and x to uniform
  prepb, prepx :: UQPL.Stmt holeT sizeT
  prepb =
    UQPL.SeqS
      [ UQPL.UnitaryS [b] UQPL.XGate
      , UQPL.UnitaryS [b] UQPL.HGate
      ]
  prepx = UQPL.UnitaryS [x] (UQPL.Unif ty)

  grover_iterate :: UQPL.Stmt holeT sizeT
  grover_iterate =
    UQPL.SeqS
      [ mk_pred x b
      , UQPL.UnitaryS [x] (UQPL.UnifDagger ty)
      , UQPL.UnitaryS [x] (UQPL.Refl0 ty)
      , UQPL.UnitaryS [x] (UQPL.Unif ty)
      ]

{- | Grover search with fail prob 0
 when the number of solutions is known
-}

-- groverCertainty ::
--   forall holeT sizeT.
--   -- | the element and type to search for. @x : T@
--   (Ident, P.VarType sizeT) ->
--   -- | number of solutions
--   sizeT ->
--   -- | the output bit
--   Ident ->
--   -- | run the predicate
--   (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
--   UQPL.Stmt holeT sizeT
-- groverCertainty (x, P.Fin n) m b mk_pred = error "TODO GroverCertainty circuit"

bruteForceQSearch ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , P.TypeCheckable sizeT
  ) =>
  -- | type of the element to search over
  P.VarType sizeT ->
  -- | call the predicate on @x, b@
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  -- | output bit
  Ident ->
  MyWriterT [UQPL.Stmt holeT sizeT] (UQPL.CompilerT primsT holeT sizeT costT) ()
bruteForceQSearch ty mk_pred out_bit = do
  x <- lift $ UQPL.allocAncilla ty
  bs <- forM (P.range ty) $ \v -> do
    b <- lift $ UQPL.allocAncilla P.tbool

    writeElem $
      UQPL.UnitaryS
        { UQPL.unitary = UQPL.RevEmbedU UQPL.ConstF{UQPL.ty = P.tbool, UQPL.val = v}
        , UQPL.args = [b]
        }
    writeElem $ mk_pred x b

    return b

  let P.Fin n = ty
  writeElem $
    UQPL.UnitaryS
      { UQPL.unitary = UQPL.RevEmbedU $ UQPL.MultiOrF n
      , UQPL.args = bs ++ [out_bit]
      }

{-# DEPRECATED useBruteForce "temporary (valid) circuit placeholder" #-}
useBruteForce :: Bool
useBruteForce = True

algoQSearchZalka ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , P.TypeCheckable sizeT
  ) =>
  -- | type of the element to search over
  P.VarType sizeT ->
  -- | call the predicate on @x, b@
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  -- | max. failure probability @\eps@
  costT ->
  -- | output bit
  Ident ->
  MyWriterT [UQPL.Stmt holeT sizeT] (UQPL.CompilerT primsT holeT sizeT costT) ()
algoQSearchZalka ty mk_pred _ out_bit | useBruteForce = bruteForceQSearch ty mk_pred out_bit
algoQSearchZalka ty mk_pred eps _out_bit = do
  writeElem qs_hole
 where
  qs_hole =
    UQPL.HoleS
      { UQPL.hole =
          QSearchBlackBox
            { q_pred_name
            , n_pred_calls = _QSearchZalka n eps
            }
      , UQPL.dagger = False
      }

  q_pred_name = case mk_pred "pred_arg" "pred_res" of
    UQPL.CallS{UQPL.proc_id} -> proc_id
    _ -> error "predicate is not a call!"
  P.Fin n = ty

  _t0 :: sizeT
  _t0 = ceiling $ logBase (4 / 3) (1 / eps) / 2

shouldUncomputeQSearch :: Bool
shouldUncomputeQSearch = False

instance
  ( Integral sizeT
  , RealFloat costT
  , holeT ~ QSearchBlackBoxes costT
  , UQPL.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  UQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive delta QSearchCFNW{predicate, return_sol = False} args [ret] = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (_1 . Ctx.at predicate)
        >>= maybeWithError ("cannot find predicate " <> predicate)

    -- size of the search space
    let s_ty@(P.Fin n) = last param_types

    -- split the precision
    let delta_search = delta / 2
    let delta_pred = delta - delta_search
    -- number of predicate queries
    let qry = _QSearchZalka n delta_search
    -- precision per predicate call
    let delta_per_pred_call = delta_pred / qry

    -- compile the predicate
    UQPL.LoweredProc
      { UQPL.lowered_def = pred_proc
      , UQPL.inp_tys = pred_inp_tys
      , UQPL.out_tys = pred_out_tys
      , UQPL.aux_tys = pred_aux_tys
      } <-
      UQPL.lowerFunDef delta_per_pred_call pred_fun

    when (pred_out_tys /= [P.tbool]) $ throwError "invalid outputs for predicate"
    when (last pred_inp_tys /= s_ty) $ throwError "mismatched search argument type"

    -- function to call the predicate, re-using the same aux space each time.
    pred_ancilla <- mapM UQPL.allocAncilla pred_aux_tys
    let pred_caller x b =
          UQPL.CallS
            { UQPL.proc_id = UQPL.proc_name pred_proc
            , UQPL.dagger = False
            , UQPL.args = args ++ [x, b] ++ pred_ancilla
            }

    -- Emit the qsearch procedure
    -- body:
    (qsearch_body, qsearch_ancilla) <- do
      ini_binds <- use UQPL.typingCtx
      ss <- execMyWriterT $ algoQSearchZalka s_ty pred_caller delta_search ret
      fin_binds <- use UQPL.typingCtx
      let ancillas = Ctx.toList $ fin_binds Ctx.\\ ini_binds
      return (UQPL.SeqS ss, ancillas)

    -- name:
    -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
    qsearch_proc_name <-
      UQPL.newIdent $
        printf "QSearch[%s, %s, %s]" (show n) (show delta_search) (UQPL.proc_name pred_proc)

    -- add the proc:
    UQPL.addProc
      UQPL.ProcDef
        { UQPL.proc_name = qsearch_proc_name
        , UQPL.proc_meta_params = []
        , UQPL.proc_params =
            UQPL.withTag UQPL.ParamInp (zip args (init pred_inp_tys))
              ++ UQPL.withTag UQPL.ParamOut [(ret, P.tbool)]
              ++ UQPL.withTag UQPL.ParamAux (zip pred_ancilla pred_aux_tys)
              ++ UQPL.withTag UQPL.ParamAux qsearch_ancilla
        , UQPL.mproc_body = Just qsearch_body
        , UQPL.is_oracle = False
        }

    if not shouldUncomputeQSearch
      then
        return
          UQPL.CallS
            { UQPL.proc_id = qsearch_proc_name
            , UQPL.args = args ++ [ret] ++ pred_ancilla ++ map fst qsearch_ancilla
            , UQPL.dagger = False
            }
      else do
        -- clean version of qsearch: uncompute to clean up ancilla
        qsearch_clean_proc_name <-
          UQPL.newIdent $
            printf "QSearch_clean[%s, %s, %s]" (show n) (show delta_search) (UQPL.proc_name pred_proc)

        out_bit <- UQPL.allocAncilla P.tbool

        UQPL.addProc
          UQPL.ProcDef
            { UQPL.proc_name = qsearch_clean_proc_name
            , UQPL.proc_meta_params = []
            , UQPL.proc_params =
                UQPL.withTag UQPL.ParamInp (zip args (init pred_inp_tys))
                  ++ UQPL.withTag UQPL.ParamOut [(ret, P.tbool)]
                  ++ UQPL.withTag UQPL.ParamAux (zip pred_ancilla pred_aux_tys)
                  ++ UQPL.withTag UQPL.ParamAux qsearch_ancilla
                  ++ UQPL.withTag UQPL.ParamAux [(out_bit, P.tbool)]
            , UQPL.mproc_body =
                Just $
                  UQPL.SeqS
                    [ UQPL.CallS
                        { UQPL.proc_id = qsearch_proc_name
                        , UQPL.args = args ++ [out_bit] ++ pred_ancilla ++ map fst qsearch_ancilla
                        , UQPL.dagger = False
                        }
                    , UQPL.UnitaryS [out_bit, ret] (UQPL.RevEmbedU $ UQPL.IdF P.tbool)
                    , UQPL.CallS
                        { UQPL.proc_id = qsearch_proc_name
                        , UQPL.args = args ++ [out_bit] ++ pred_ancilla ++ map fst qsearch_ancilla
                        , UQPL.dagger = True
                        }
                    ]
            , UQPL.is_oracle = False
            }

        return
          UQPL.CallS
            { UQPL.proc_id = qsearch_clean_proc_name
            , UQPL.args = args ++ [ret] ++ pred_ancilla ++ map fst qsearch_ancilla ++ [out_bit]
            , UQPL.dagger = False
            }

  -- fallback
  lowerPrimitive _ _ _ _ = throwError "Unsupported"

-- ================================================================================
-- CQ Lowering
-- ================================================================================

allocReg :: Ident -> P.VarType sizeT -> MyWriterT ([CQPL.Stmt holeT sizeT], [(Ident, P.VarType sizeT)]) (CQPL.CompilerT primsT holeT sizeT costT) Ident
allocReg prefix ty = do
  reg <- lift $ CQPL.newIdent prefix
  writeElemAt _2 (reg, ty)
  return reg

-- | Implementation of the hybrid quantum search algorithm \( \textbf{QSearch} \).
algoQSearch ::
  forall primsT holeT sizeT costT.
  ( Integral sizeT
  , RealFloat costT
  , sizeT ~ SizeT
  , holeT ~ QSearchBlackBoxes costT
  , CQPL.Lowerable primsT primsT holeT sizeT costT
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
  MyWriterT ([CQPL.Stmt holeT sizeT], [(Ident, P.VarType sizeT)]) (CQPL.CompilerT primsT holeT sizeT costT) ()
algoQSearch ty n_samples eps grover_k_caller pred_caller ok = do
  not_done <- allocReg "not_done" P.tbool
  q_sum <- allocReg "Q_sum" j_type
  j <- allocReg "j" j_type
  j_lim <- allocReg "j_lim" j_type
  x <- allocReg "x" ty

  -- classical sampling
  when (n_samples /= 0) $ do
    let classicalSampling =
          CQPL.WhileKWithCondExpr (CQPL.MetaSize n_samples) not_done (CQPL.NotE $ CQPL.VarE ok) $
            CQPL.SeqS
              [ CQPL.RandomS x (UQPL.MetaSize n)
              , pred_caller x ok
              ]
    writeElemAt _1 classicalSampling

  -- quantum search

  -- one call and meas to grover with j iterations
  let quantumGroverOnce =
        CQPL.SeqS
          [ CQPL.RandomDynS j j_lim
          , CQPL.AssignS [q_sum] (CQPL.AddE (CQPL.VarE q_sum) (CQPL.VarE j))
          , CQPL.AssignS
              [not_done]
              ( CQPL.AndE
                  (CQPL.VarE not_done)
                  (CQPL.LEqE (CQPL.VarE q_sum) (CQPL.VarE j_lim))
              )
          , CQPL.ifThenS
              not_done
              ( CQPL.SeqS
                  [ grover_k_caller (Right j) x ok
                  , pred_caller x ok
                  , CQPL.AssignS [not_done] (CQPL.AndE (CQPL.VarE not_done) (CQPL.NotE $ CQPL.VarE ok))
                  ]
              )
          ]

  let quantumSamplingOneRound =
        CQPL.SeqS
          [ CQPL.AssignS [q_sum] (CQPL.ConstE{CQPL.val = 0, CQPL.val_ty = j_type})
          , CQPL.ForInArray
              { CQPL.loop_index = j_lim
              , CQPL.loop_index_ty = j_type
              , CQPL.loop_values = map CQPL.MetaSize sampling_ranges
              , CQPL.loop_body = quantumGroverOnce
              }
          ]

  let quantumSampling = CQPL.RepeatS (CQPL.MetaSize n_runs) quantumSamplingOneRound

  writeElemAt _1 quantumSampling
 where
  P.Fin n = ty

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
  , CQPL.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  CQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive eps QSearchCFNW{predicate, return_sol = False} args [ret] = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (_1 . Ctx.at predicate)
        >>= maybeWithError ("cannot find predicate " <> predicate)

    -- size of the search space
    let s_ty@(P.Fin n) = last param_types

    -- fail prob of search
    let eps_s = eps / 2

    -- fail prob predicate
    let eps_pred = eps - eps_s
    let n_max_pred_calls = _EQSearchWorst n eps_pred
    let eps_per_pred_call = eps_pred / n_max_pred_calls
    let delta_per_pred_call = eps_per_pred_call / 2 -- norm error in unitary predicate

    -- lower the unitary predicate
    let upred_compiler = UQPL.lowerFunDef delta_per_pred_call pred_fun
    (pred_uproc, uprocs) <- do
      uenv <- view id
      ust <- use id
      (a, _, w) <- lift $ runMyReaderWriterStateT upred_compiler uenv ust
      return (a, w)

    tellAt CQPL.loweredUProcs uprocs
    let UQPL.LoweredProc
          { UQPL.inp_tys = pred_inp_tys
          , UQPL.out_tys = pred_out_tys
          , UQPL.aux_tys = pred_aux_tys
          } = pred_uproc
    let upred_proc_name = pred_uproc ^. to UQPL.lowered_def . to UQPL.proc_name

    -- make the Grover_k uproc
    -- TODO this should ideally be done by algoQSearch, but requires a lot of aux information.
    uproc_grover_k_name <- CQPL.newIdent "Grover"
    upred_aux_vars <- replicateM (length pred_aux_tys) $ CQPL.newIdent "aux"
    grover_arg_name <- CQPL.newIdent "x"
    let meta_k = UQPL.MetaName "k"
    let uproc_grover_k_body =
          groverK
            meta_k
            (grover_arg_name, s_ty)
            ret
            ( \x b ->
                UQPL.CallS
                  { UQPL.proc_id = upred_proc_name
                  , UQPL.dagger = False
                  , UQPL.args = args ++ [x, b] ++ upred_aux_vars
                  }
            )
    let uproc_grover_k =
          UQPL.ProcDef
            { UQPL.proc_name = uproc_grover_k_name
            , UQPL.proc_meta_params = ["k"]
            , UQPL.proc_params =
                UQPL.withTag UQPL.ParamInp (zip (args ++ [grover_arg_name]) pred_inp_tys)
                  ++ UQPL.withTag UQPL.ParamOut [(ret, P.tbool)]
                  ++ UQPL.withTag UQPL.ParamAux (zip upred_aux_vars pred_aux_tys)
            , UQPL.mproc_body = Just uproc_grover_k_body
            , UQPL.is_oracle = False
            }
    writeElemAt CQPL.loweredUProcs uproc_grover_k

    let grover_k_caller k x b =
          CQPL.CallS
            { CQPL.fun = CQPL.UProcAndMeas uproc_grover_k_name
            , CQPL.meta_params = [k]
            , CQPL.args = [x, b]
            }

    -- emit the QSearch algorithm
    qsearch_params <- forM (args ++ [ret]) $ \x -> do
      ty <- use $ CQPL.typingCtx . Ctx.at x . singular _Just
      return (x, ty)

    -- let upred_caller = (\x b -> UQPL.holeS $ TODOHole $ printf "unitary predicate call (%s, %s)" x b)

    let pred_caller =
          ( \x b ->
              CQPL.CallS
                { CQPL.fun = CQPL.UProcAndMeas upred_proc_name
                , CQPL.meta_params = []
                , CQPL.args = args ++ [x, b]
                }
          )

    (qsearch_body, qsearch_local_vars) <- execMyWriterT $ algoQSearch s_ty 0 eps_s grover_k_caller pred_caller ret
    qsearch_proc_name <- CQPL.newIdent $ printf "QSearch[%s]" (show eps_s)
    CQPL.addProc $
      CQPL.ProcDef
        { CQPL.proc_name = qsearch_proc_name
        , CQPL.proc_meta_params = []
        , CQPL.proc_param_types = map snd qsearch_params
        , CQPL.mproc_body =
            Just $
              CQPL.ProcBody
                { CQPL.proc_param_names = map fst qsearch_params
                , CQPL.proc_local_vars = qsearch_local_vars
                , CQPL.proc_body_stmt = CQPL.SeqS qsearch_body
                }
        , CQPL.is_oracle = False
        }

    return
      CQPL.CallS
        { CQPL.fun = CQPL.FunctionCall qsearch_proc_name
        , CQPL.args = args ++ [ret]
        , CQPL.meta_params = []
        }
  lowerPrimitive _ _ _ _ = error "Unsupported"

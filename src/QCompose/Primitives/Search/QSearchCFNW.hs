{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives.Search.QSearchCFNW (
  -- * Search Primitive
  QSearchCFNW (..),

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
  returnsSol = return_sol

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
  typeCheckPrimitive prim args = do
    let predicate = getPredicate prim
    P.FunDef{P.param_types, P.ret_types} <-
      view (Ctx.at predicate)
        >>= maybeWithError (printf "cannot find search predicate `%s`" predicate)

    when (ret_types /= [P.tbool]) $
      throwError "predicate must return a single Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init param_types /= arg_tys) $
      throwError "Invalid arguments to bind to predicate"

    return $ P.tbool : [last param_types | returnsSol prim]

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

instance
  ( Integral sizeT
  , Floating costT
  , UQPL.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  UQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive delta QSearchCFNW{predicate, return_sol = False} args rets = do
    -- the predicate
    pred_fun@P.FunDef{P.param_types} <-
      view (_1 . Ctx.at predicate)
        >>= maybeWithError ("cannot find predicate " <> predicate)

    -- size of the search space
    let s_ty@(P.Fin n) = last param_types

    -- precision for the search
    let delta_search = delta / 2
    -- precision for each predicate call
    let n_qry = _QSearchZalka n delta_search
    let delta_per_pred_call = (delta - delta_search) / n_qry

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

    -- emit the qsearch procedure
    -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
    qsearch_proc_name <-
      UQPL.newIdent $
        printf "QSearch[%s, %s, %s]" (show n) (show delta_search) (UQPL.proc_name pred_proc)
    qsearch_ancilla_tys <- error "TODO ancilla types" -- view (UQPL.qsearchConfig . to UQPL.ancillaTypes) <&> (\f -> f n delta_search)
    let qsearch_param_tys =
          init pred_inp_tys
            ++ pred_out_tys
            ++ pred_aux_tys
            ++ qsearch_ancilla_tys

    qsearch_ancilla <- mapM UQPL.allocAncilla qsearch_ancilla_tys
    pred_ancilla <- mapM UQPL.allocAncilla pred_aux_tys

    qsearch_param_names <- replicateM (length qsearch_param_tys) $ UQPL.newIdent "_qs"
    mk_proc_body <- error "view $ UQPL.qsearchConfig . to UQPL.algorithm"
    let proc_body =
          mk_proc_body
            s_ty
            (\x b -> UQPL.CallS{UQPL.proc_id = UQPL.proc_name pred_proc, UQPL.dagger = False, UQPL.args = args ++ [x, b] ++ pred_ancilla})
            delta_search
    UQPL.addProc
      UQPL.ProcDef
        { UQPL.proc_name = qsearch_proc_name
        , UQPL.proc_params = UQPL.withTag UQPL.ParamUnk $ zip qsearch_param_names qsearch_param_tys
        , UQPL.mproc_body = Just proc_body
        , UQPL.is_oracle = False
        }

    return
      UQPL.CallS
        { UQPL.proc_id = qsearch_proc_name
        , UQPL.args = args ++ rets ++ pred_ancilla ++ qsearch_ancilla
        , UQPL.dagger = False
        }
  lowerPrimitive _ _ _ _ = error "TODO"

instance
  ( Integral sizeT
  , Floating costT
  , CQPL.Lowerable primsT primsT holeT sizeT costT
  , Show sizeT
  , Show costT
  , P.TypeCheckable sizeT
  ) =>
  CQPL.Lowerable primsT QSearchCFNW holeT sizeT costT
  where
  lowerPrimitive eps QSearchCFNW{predicate, return_sol = False} args rets = do
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
    max_cost_formula <- error "view $ CQPL.qsearchConfig . to CQPL.costFormulas . to P.qSearchWorstCaseCost"
    let n_max_pred_calls = max_cost_formula n eps_pred
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
    let pred_proc_name = pred_uproc ^. to UQPL.lowered_def . to UQPL.proc_name

    -- emit the QSearch algorithm
    qsearch_builder <- error "view $ CQPL.qsearchConfig . to CQPL.qsearchAlgo"
    qsearch_params <- forM (args ++ rets) $ \x -> do
      ty <- use $ CQPL.typingCtx . Ctx.at x . singular _Just
      return (x, ty)
    let qsearch_proc =
          qsearch_builder
            s_ty
            0
            eps_s
            (\x b -> error "TODO unitary pred call")
            (\x b -> CQPL.HoleS "classical predicate call")
            qsearch_params
    qsearch_proc_name <- CQPL.newIdent $ printf "QSearch[%s]" (show eps_s)
    CQPL.addProc $ qsearch_proc{CQPL.proc_name = qsearch_proc_name}

    return
      CQPL.CallS
        { CQPL.fun = CQPL.FunctionCall qsearch_proc_name
        , CQPL.args = args ++ rets
        , CQPL.meta_params = []
        }
  lowerPrimitive _ _ _ _ = error "TODO"

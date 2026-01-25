{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class (
  Primitive (..),
  module Traq.Primitives.Class.Prelude,
  module Traq.Primitives.Class.Serialize,
  module Traq.Primitives.Class.TypeCheck,
  module Traq.Primitives.Class.Eval,
  module Traq.Primitives.Class.UnitaryCost,
  module Traq.Primitives.Class.QuantumCost,
  module Traq.Primitives.Class.UnitaryCompile,
) where

import Control.Applicative (Alternative ((<|>)), many)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec (try)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class.Eval
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.QuantumCost
import Traq.Primitives.Class.Serialize
import Traq.Primitives.Class.TypeCheck
import Traq.Primitives.Class.UnitaryCompile
import Traq.Primitives.Class.UnitaryCost
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

{- | A generic second-order primitive.
It accepts a sequence of partially applied functions.

Authors are provided with simpler type classes to implement the features:
typing, semantics and query costs.
-}
data Primitive prim = Primitive [PartialFun] prim
  deriving (Eq, Show)

type instance SizeType (Primitive p) = SizeType p
type instance PrecType (Primitive p) = PrecType p

-- ================================================================================
-- Instances
-- ================================================================================

instance P.RenameVars (Primitive p) where
  renameVars pref (Primitive par_funs p) = Primitive (map (P.renameVars pref) par_funs) p

instance (P.MapSize prim) => P.MapSize (Primitive prim) where
  type MappedSize (Primitive prim) size' = Primitive (P.MappedSize prim size')
  mapSize f (Primitive par_funs prim) = Primitive par_funs (P.mapSize f prim)

instance P.HasFreeVars (Primitive prim) where
  freeVarsList (Primitive par_funs _) = concatMap (catMaybes . pfun_args) par_funs

-- Pretty Printing
instance (SerializePrim prim) => PP.ToCodeString (Primitive prim) where
  build (Primitive par_funs prim) = do
    fns <- concatMapM PP.fromBuild par_funs
    PP.putWord $ printf "@%s<%s>[%s]" (primNameOf prim) (PP.commaList $ printPrimParams prim) fns

-- Parsing
instance (SerializePrim prim, SizeType prim ~ Sym.Sym SizeT) => P.Parseable (Primitive prim) where
  parseE tp@TokenParser{..} = do
    ('@' : name) <- foldr1 (<|>) $ map (\s -> try $ symbol $ "@" ++ s) $ primNames @prim
    prim <- angles $ parsePrimParams tp name
    par_funs <- brackets $ many $ P.parseE tp
    return $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Typing
-- --------------------------------------------------------------------------------

instance (TypeCheckPrim prim size, P.TypingReqs size) => P.TypeInferrable (Primitive prim) size where
  inferTypes (Primitive par_funs prim) = do
    fn_tys <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      P.FunDef{P.param_types, P.ret_types} <-
        view (Ctx.at pfun_name)
          >>= maybeWithError (printf "cannot find function argument `%s`" pfun_name)

      when (length pfun_args /= length param_types) $
        throwError "Invalid number of function arguments"

      prim_arg_tys <- forM (zip pfun_args param_types) $ \(mvar, ty) -> do
        case mvar of
          Just var -> do
            var_ty <- Ctx.lookup var
            when (var_ty /= ty) $ throwError "invalid arg type to bind"
            return Nothing
          Nothing -> return $ Just ty

      return $ P.FnType (catMaybes prim_arg_tys) ret_types

    shaped_fn_tys <- liftEither $ listToShape fn_tys

    inferRetTypesPrim prim shaped_fn_tys

-- --------------------------------------------------------------------------------
-- Eval
-- --------------------------------------------------------------------------------

instance
  ( EvalPrim prim size prec
  , P.EvalReqs size prec
  ) =>
  P.Evaluatable (Primitive prim) size prec
  where
  eval (Primitive par_funs prim) sigma = do
    fns_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' = P.eval1 P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn} (placeArgs vs vs')
      return eval_fn

    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    evalPrim prim shaped_fns_eval

-- ================================================================================
-- Analysis (Unitary)
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Error
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , A.ErrorReqs size prec
  ) =>
  A.TraceNormErrorU (A.AnnFailProb (Primitive prim)) size prec
  where
  traceNormErrorU (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps
    eps_fn <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      A.traceNormErrorU fn

    let tot_eps_fns = sum $ zipWith A.unitarySubroutineTVErrorTotal query_costs eps_fn
    return $ eps + tot_eps_fns

-- --------------------------------------------------------------------------------
-- Cost
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , A.CostReqs size prec
  ) =>
  A.CostU (A.AnnFailProb (Primitive prim)) size prec
  where
  costU (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps

    fn_costs <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      A.costU1 $ P.NamedFunDef pfun_name fn

    -- all other non-query operations
    let extra_costs = unitaryExprCosts prim eps

    return $ Alg.sum $ extra_costs : zipWith (Alg..*) query_costs fn_costs

-- --------------------------------------------------------------------------------
-- Annotation
-- --------------------------------------------------------------------------------

instance
  (UnitaryCostPrim prim size prec) =>
  A.AnnotateWithErrorBudgetU (Primitive prim)
  where
  annEpsU eps (A.AnnFailProb eps_old (Primitive par_funs prim)) = do
    -- check if any of the functions can error
    pfuns_may_error <- fmap or $ forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      A.canError fn

    -- split the overall precision in half if fns can error, otherwise use full.
    let eps_alg = min eps_old (if pfuns_may_error then A.splitFailProb eps 2 else eps)

    let n_queries_u = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.splitFailProb (eps - eps_alg) (fromIntegral $ length par_funs)

    forM_ (zip par_funs n_queries_u) $ \(PartialFun{pfun_name}, n_query_u) -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = P.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns n_query_u
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Compilation
-- --------------------------------------------------------------------------------

-- instance
--   (UnitaryCompilePrim prim (SizeType prim) (PrecType prim)) =>
--   Compiler.CompileU (A.AnnFailProb (Primitive prim))
--   where
--   compileU (A.AnnFailProb eps (Primitive par_funs prim)) rets = do
--     error "TODO compileU Prim"

-- ================================================================================
-- Analysis (Quantum)
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Error
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , A.ErrorReqs size prec
  ) =>
  A.TVErrorQ (A.AnnFailProb (Primitive prim)) size prec
  where
  tvErrorQ (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps
    let query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps

    eps_fns <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      eps_q <- A.tvErrorQ fn
      eps_u <- A.traceNormErrorU fn
      return (eps_q, eps_u)
    let (eps_fns_q, eps_fns_u) = unzip eps_fns

    let tot_eps_u = sum $ zipWith A.unitarySubroutineTVErrorTotal query_costs_u eps_fns_u
    let tot_eps_q = sum $ zipWith calc_tot_err_q query_costs_q eps_fns_q
    return $ eps + tot_eps_q + tot_eps_u
   where
    calc_tot_err_q :: prec -> A.FailProb prec -> A.FailProb prec
    calc_tot_err_q n eps' = A.failProb $ n * A.getFailProb eps'

-- --------------------------------------------------------------------------------
-- Cost (worst/havoc)
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , A.CostReqs size prec
  ) =>
  A.CostQ (A.AnnFailProb (Primitive prim)) size prec
  where
  costQ (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps
    let query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps

    fn_costs_uq <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      cost_u <- A.costU1 $ P.NamedFunDef pfun_name fn
      cost_q <- A.costQ1 $ P.NamedFunDef pfun_name fn
      return (cost_u, cost_q)
    let (fn_costs_u, fn_costs_q) = unzip fn_costs_uq

    let tot_cost_q = Alg.sum $ zipWith (Alg..*) query_costs_q fn_costs_q
    let tot_cost_u = Alg.sum $ zipWith (Alg..*) query_costs_u fn_costs_u

    -- all other non-query operations
    let extra_costs = quantumExprCosts prim eps

    return $ Alg.sum [tot_cost_q, tot_cost_u, extra_costs]

-- --------------------------------------------------------------------------------
-- Cost (expected)
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , QuantumExpCostPrim prim size prec
  , EvalPrim prim size prec
  , A.CostReqs size prec
  , P.EvalReqs size prec
  ) =>
  A.ExpCostQ (A.AnnFailProb (Primitive prim)) size prec
  where
  expCostQ (A.AnnFailProb eps (Primitive par_funs prim)) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view P._evaluationEnv

    fns_with_args_and_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' =
            P.eval1 P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn} (placeArgs vs vs')
              & (runReaderT ?? eval_env)

      return ((P.NamedFunDef pfun_name fn, vs), eval_fn)

    let (fns_with_args, fns_eval) = unzip fns_with_args_and_eval
    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    -- expected queries
    let exp_query_costs_q = shapeToList $ quantumExpQueryCostsQuantum prim eps shaped_fns_eval
    let exp_query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumExpQueryCostsUnitary prim eps shaped_fns_eval

    fn_costs <- forM (zip3 fns_with_args exp_query_costs_q exp_query_costs_u) $
      \((fn, pref_args), exp_queries_q, exp_queries_u) -> do
        -- queries to quantum f
        q_costs <- forM exp_queries_q $ \(vs, eq) -> do
          q_f <- A.expCostQ1 fn (placeArgs pref_args vs)
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- A.costU1 fn
        let u_cost = exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs

-- --------------------------------------------------------------------------------
-- Annotation
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  ) =>
  A.AnnotateWithErrorBudgetQ (Primitive prim)
  where
  annEpsQ eps (A.AnnFailProb eps_old (Primitive par_funs prim)) = do
    -- check if any of the functions can error
    pfuns_may_error <- fmap or $ forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      A.canError fn

    -- split the overall precision in half if fns can error, otherwise use full.
    let eps_alg = min eps_old (if pfuns_may_error then A.splitFailProb eps 2 else eps)

    let n_queries_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let n_queries_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.splitFailProb (eps - eps_alg) (fromIntegral $ length par_funs)

    forM_ (zip3 par_funs n_queries_q n_queries_u) $ \(PartialFun{pfun_name}, n_query_q, n_query_u) -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = P.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns (n_query_q + n_query_u)
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_q > 0) $ void $ A.annEpsQ1 eps_fn named_fn
      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

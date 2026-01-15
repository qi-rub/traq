{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.QuantumCost (
  -- ** Classical-Quantum Compilation and Cost
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),
) where

import Control.Monad (forM, forM_, void, when)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import Traq.Primitives.Class.Eval
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.TypeCheck
import Traq.Primitives.Class.UnitaryCost
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Quantum Compiler: Costs, Error.
-- --------------------------------------------------------------------------------

{- | Worst-case Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , A.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumHavocCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function's quantum compilation.
  quantumQueryCostsQuantum :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Bound on number of queries made to each function's unitary compilation.
  quantumQueryCostsUnitary :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  quantumExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  quantumExprCosts _ _ = Alg.zero

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
      cost_u <- A.costU $ P.NamedFunDef pfun_name fn
      cost_q <- A.costQ $ P.NamedFunDef pfun_name fn
      return (cost_u, cost_q)
    let (fn_costs_u, fn_costs_q) = unzip fn_costs_uq

    let tot_cost_q = Alg.sum $ zipWith (Alg..*) query_costs_q fn_costs_q
    let tot_cost_u = Alg.sum $ zipWith (Alg..*) query_costs_u fn_costs_u
    -- TODO expression cost
    return $ Alg.sum [tot_cost_q, tot_cost_u]

instance
  ( QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.TypingReqs size
  , Floating prec
  , Ord prec
  ) =>
  A.QuantumHavocCost (Primitive prim) size prec
  where
  quantumHavocCost eps (Primitive par_funs prim) = do
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.divideError (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip3 par_funs query_costs_q query_costs_u) $ \(PartialFun{pfun_name}, n_queries_q, n_queries_u) -> do
      -- divide by number of queries to get cost per call
      let eps_fn = A.divideError eps_fns (n_queries_u + n_queries_q)

      -- cost per call to f, with the same precision.
      cost_f_q <- A.quantumMaxQueryCostF eps_fn pfun_name
      cost_f_u <- A.unitaryQueryCostF (A.requiredFailProbToNormError eps_fn) pfun_name

      return $ (n_queries_q Alg..* cost_f_q) Alg.+ (2 * n_queries_u Alg..* cost_f_u)

    -- all other non-query operations
    let extra_costs = unitaryExprCosts prim eps_alg

    return $ Alg.sum fn_costs Alg.+ extra_costs

{- | Expected Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , A.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumExpCostPrim prim size prec
  where
  {- Bound on the expected number of queries made to each function's quantum compilation.
  This is a random variable over the inputs to each function.
  -}
  quantumExpQueryCostsQuantum ::
    forall shape m.
    ( shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape [([P.Value size], prec)]

  -- | Bound on the expected number of queries made to each function's unitary compilation.
  quantumExpQueryCostsUnitary ::
    forall shape m.
    (shape ~ PrimFnShape prim, m ~ P.EvaluationMonad prec) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  quantumExpExprCosts ::
    forall shape cost m.
    ( C.CostModel cost
    , prec ~ PrecType cost
    , shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    cost
  quantumExpExprCosts _ _ _ = Alg.zero

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
            P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
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
          let sigma_fn =
                Ctx.fromList $
                  zip
                    [show i | i <- [0 :: Int ..]]
                    (placeArgs pref_args vs)
          q_f <- A.expCostQ fn sigma_fn
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- A.costU fn
        let u_cost = exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs

instance
  ( QuantumExpCostPrim prim size prec
  , EvalPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.EvalReqs size prec
  , Floating prec
  , Ord prec
  ) =>
  A.QuantumExpCost (Primitive prim) size prec
  where
  quantumExpCost eps (Primitive par_funs prim) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view P._evaluationEnv

    fns_eval_and_args <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' =
            P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
              & (runReaderT ?? eval_env)

      return (eval_fn, vs)

    let (fns_eval, fns_args) = unzip fns_eval_and_args
    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    -- Compute the expected cost
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    -- worst case queries
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- expected queries
    let exp_query_costs_q = shapeToList $ quantumExpQueryCostsQuantum prim eps_alg shaped_fns_eval
    let exp_query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumExpQueryCostsUnitary prim eps_alg shaped_fns_eval

    -- split the other half into equal parts per function
    let eps_fns = A.divideError (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip3 (zip par_funs fns_args) (zip query_costs_q exp_query_costs_q) (zip query_costs_u exp_query_costs_u)) $
      \((PartialFun{pfun_name}, pref_args), (n_queries_q, exp_queries_q), (n_queries_u, exp_queries_u)) -> do
        -- divide by number of queries to get cost per call
        let eps_fn = A.divideError eps_fns (n_queries_u + n_queries_q)

        -- queries to quantum f
        q_costs <- forM exp_queries_q $ \(vs, eq) -> do
          q_f <- A.quantumQueryCostF eps_fn (placeArgs pref_args vs) pfun_name
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- A.unitaryQueryCostF (A.requiredFailProbToNormError eps_fn) pfun_name
        let u_cost = 2 * exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps_alg shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs

-- --------------------------------------------------------------------------------
-- Annotation
-- --------------------------------------------------------------------------------

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  ) =>
  A.AnnotateWithErrorBudget (Primitive prim)
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
    let eps_fns = A.splitFailProb (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    forM_ (zip par_funs n_queries_u) $ \(PartialFun{pfun_name}, n_query_u) -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = P.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns n_query_u
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

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
    let eps_fns = A.splitFailProb (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    forM_ (zip3 par_funs n_queries_q n_queries_u) $ \(PartialFun{pfun_name}, n_query_q, n_query_u) -> do
      fn <- use $ P._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = P.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns (n_query_q + n_query_u)
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_q > 0) $ void $ A.annEpsQ1 eps_fn named_fn
      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

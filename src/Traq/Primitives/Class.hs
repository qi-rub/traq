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
  module Traq.Primitives.Class.Compile,
) where

import Control.Applicative (Alternative ((<|>)), many)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (censor)
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
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class.Compile
import Traq.Primitives.Class.Eval
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.QuantumCost
import Traq.Primitives.Class.Serialize
import Traq.Primitives.Class.TypeCheck
import Traq.Primitives.Class.UnitaryCost
import qualified Traq.QPL as QPL
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

instance CPL.RenameVars (Primitive p) where
  renameVars pref (Primitive par_funs p) = Primitive (map (CPL.renameVars pref) par_funs) p

instance (CPL.MapSize prim) => CPL.MapSize (Primitive prim) where
  type MappedSize (Primitive prim) size' = Primitive (CPL.MappedSize prim size')
  mapSize f (Primitive par_funs prim) = Primitive par_funs (CPL.mapSize f prim)

instance CPL.HasFreeVars (Primitive prim) where
  freeVarsList (Primitive par_funs _) = concatMap (catMaybes . pfun_args) par_funs

-- Pretty Printing
instance (SerializePrim prim) => PP.ToCodeString (Primitive prim) where
  build (Primitive par_funs prim) = do
    fns <- concatMapM PP.fromBuild par_funs
    PP.putWord $ printf "@%s<%s>[%s]" (primNameOf prim) (PP.commaList $ printPrimParams prim) fns

-- Parsing
instance (SerializePrim prim, SizeType prim ~ Sym.Sym SizeT) => CPL.Parseable (Primitive prim) where
  parseE tp@TokenParser{..} = do
    ('@' : name) <- foldr1 (<|>) $ map (\s -> try $ symbol $ "@" ++ s) $ primNames @prim
    prim <- angles $ parsePrimParams tp name
    par_funs <- brackets $ many $ CPL.parseE tp
    return $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Typing
-- --------------------------------------------------------------------------------

instance (TypeCheckPrim prim size, CPL.TypingReqs size) => CPL.TypeInferrable (Primitive prim) size where
  inferTypes (Primitive par_funs prim) = do
    fn_tys <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      CPL.FunDef{CPL.param_types, CPL.ret_types} <-
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

      return $ CPL.FnType (catMaybes prim_arg_tys) ret_types

    shaped_fn_tys <- liftEither $ listToShape fn_tys

    inferRetTypesPrim prim shaped_fn_tys

-- --------------------------------------------------------------------------------
-- Eval
-- --------------------------------------------------------------------------------

instance
  ( EvalPrim prim size prec
  , CPL.EvalReqs size prec
  ) =>
  CPL.Evaluatable (Primitive prim) size prec
  where
  eval (Primitive par_funs prim) sigma = do
    fns_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          CPL._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' = CPL.eval1 CPL.NamedFunDef{CPL.fun_name = pfun_name, CPL.fun_def = fn} (placeArgs vs vs')
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
      fn <- view $ CPL._funCtx . Ctx.at pfun_name . non' (error "invalid function")
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
      fn <- view $ CPL._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      A.costU1 $ CPL.NamedFunDef pfun_name fn

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
      fn <- use $ CPL._funCtx . Ctx.at pfun_name . singular _Just
      A.canError fn

    -- split the overall precision in half if fns can error, otherwise use full.
    let eps_alg = min eps_old (if pfuns_may_error then A.splitFailProb eps 2 else eps)

    let n_queries_u = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.splitFailProb (eps - eps_alg) (fromIntegral $ length par_funs)

    forM_ (zip par_funs n_queries_u) $ \(PartialFun{pfun_name}, n_query_u) -> do
      fn <- use $ CPL._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = CPL.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns n_query_u
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Compilation
-- --------------------------------------------------------------------------------

prependBoundArgs ::
  [Ident] ->
  [(Ident, CPL.VarType size)] ->
  QPL.ProcDef size ->
  QPL.ProcDef size
prependBoundArgs pfun_names bound_args QPL.ProcDef{..} =
  QPL.ProcDef
    { QPL.proc_param_types = map snd bound_args ++ proc_param_types
    , QPL.proc_body = go proc_body
    , ..
    }
 where
  bound_arg_names = map fst bound_args

  go :: QPL.ProcBody size -> QPL.ProcBody size
  go (QPL.ProcBodyU QPL.UProcBody{..}) =
    QPL.ProcBodyU $
      QPL.UProcBody
        { QPL.uproc_param_names = bound_arg_names ++ uproc_param_names
        , QPL.uproc_param_tags = replicate (length bound_args) QPL.ParamUnk ++ uproc_param_tags
        , QPL.uproc_body_stmt = goUStmt uproc_body_stmt
        }
  go (QPL.ProcBodyC QPL.CProcBody{..}) =
    QPL.ProcBodyC $
      QPL.CProcBody
        { QPL.cproc_param_names = bound_arg_names ++ cproc_param_names
        , QPL.cproc_body_stmt = goStmt cproc_body_stmt
        , ..
        }
  go _ = error "invalid procs"

  goUStmt :: QPL.UStmt size -> QPL.UStmt size
  goUStmt (QPL.USeqS ss) = QPL.USeqS (map goUStmt ss)
  goUStmt s@QPL.UCallS{QPL.uproc_id, QPL.qargs}
    | uproc_id `notElem` pfun_names = s{QPL.qargs = map QPL.Arg bound_arg_names ++ qargs}
  goUStmt (QPL.URepeatS n body) = QPL.URepeatS n (goUStmt body)
  goUStmt s@QPL.UForInRangeS{QPL.uloop_body} = s{QPL.uloop_body = goUStmt uloop_body}
  goUStmt s@QPL.UWithComputedS{QPL.with_ustmt, QPL.body_ustmt} =
    s{QPL.with_ustmt = goUStmt with_ustmt, QPL.body_ustmt = goUStmt body_ustmt}
  goUStmt s = s

  goStmt :: QPL.Stmt size -> QPL.Stmt size
  goStmt (QPL.SeqS ss) = QPL.SeqS (map goStmt ss)
  goStmt s@QPL.CallS{QPL.fun, QPL.args}
    | callTarget fun `notElem` pfun_names = s{QPL.args = map QPL.Arg bound_arg_names ++ args}
  goStmt s@QPL.IfThenElseS{QPL.s_true, QPL.s_false} =
    s{QPL.s_true = goStmt s_true, QPL.s_false = goStmt s_false}
  goStmt s@QPL.RepeatS{QPL.loop_body} = s{QPL.loop_body = goStmt loop_body}
  goStmt s@QPL.WhileK{QPL.loop_body} = s{QPL.loop_body = goStmt loop_body}
  goStmt s@QPL.WhileKWithCondExpr{QPL.loop_body} = s{QPL.loop_body = goStmt loop_body}
  goStmt s@QPL.ForInArray{QPL.loop_body} = s{QPL.loop_body = goStmt loop_body}
  goStmt s = s

  callTarget :: QPL.FunctionCall -> Ident
  callTarget (QPL.FunctionCall name) = name
  callTarget (QPL.UProcAndMeas name) = name

instance
  ( TypeCheckPrim prim (SizeType prim)
  , CPL.TypingReqs (SizeType prim)
  , UnitaryCompilePrim prim (SizeType prim) (PrecType prim)
  ) =>
  Compiler.CompileU (A.AnnFailProb (Primitive prim))
  where
  compileU (A.AnnFailProb eps (Primitive par_funs prim)) rets = do
    let pfun_names = map pfun_name par_funs
    let bound_args_names = concatMap (catMaybes . pfun_args) par_funs
    bound_args_tys <- forM bound_args_names $ \x -> use $ CPL._typingCtx . Ctx.at x . non' (error $ "invalid arg " ++ x)
    let bound_args = zip bound_args_names bound_args_tys

    mk_ucall <-
      reshape $
        par_funs <&> \PartialFun{pfun_name, pfun_args} xs ->
          QPL.UCallS
            { uproc_id = Compiler.mkUProcName pfun_name
            , dagger = False
            , qargs = placeArgsWithExcess (map (fmap QPL.Arg) pfun_args) xs
            }

    uproc_aux_types <-
      reshape =<< do
        forM par_funs $ \PartialFun{pfun_name} -> do
          let uproc_name = Compiler.mkUProcName pfun_name
          sign <-
            use (Compiler._procSignatures . at uproc_name)
              >>= maybeWithError (printf "could not find uproc `%s` for fun `%s`" uproc_name pfun_name)
          return $ Compiler.aux_tys sign

    prim_ret_types <- forM rets $ \x ->
      use (CPL._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

    let builder =
          PrimCompileEnv
            { mk_ucall
            , mk_call = reshapeUnsafe $ replicate (length par_funs) (error "cannot call proc from UPrim")
            , mk_meas = reshapeUnsafe $ replicate (length par_funs) (error "cannot meas uproc from UPrim")
            , uproc_aux_types
            , prim_ret_types
            }
    let arg_bounder = prependBoundArgs (map Compiler.mkUProcName pfun_names) bound_args
    prim_proc_raw <-
      runReaderT (compileUPrim prim eps) builder
        & censor (Compiler._loweredProcs . each %~ arg_bounder)
    let prim_proc = arg_bounder prim_proc_raw
    Compiler.addProc prim_proc

    let prim_aux_tys =
          prim_proc
            & QPL.proc_param_types
            & drop (length bound_args + length rets)
    prim_aux_vars <- mapM (Compiler.allocAncillaWithPref "aux_prim") prim_aux_tys
    return $
      QPL.UCallS
        { QPL.uproc_id = QPL.proc_name prim_proc
        , QPL.qargs = map (QPL.Arg . fst) bound_args ++ map QPL.Arg rets ++ map QPL.Arg prim_aux_vars
        , QPL.dagger = False
        }

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
      fn <- view $ CPL._funCtx . Ctx.at pfun_name . non' (error "invalid function")
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
      fn <- view $ CPL._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      cost_u <- A.costU1 $ CPL.NamedFunDef pfun_name fn
      cost_q <- A.costQ1 $ CPL.NamedFunDef pfun_name fn
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
  , CPL.EvalReqs size prec
  ) =>
  A.ExpCostQ (A.AnnFailProb (Primitive prim)) size prec
  where
  expCostQ (A.AnnFailProb eps (Primitive par_funs prim)) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view CPL._evaluationEnv

    fns_with_args_and_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          CPL._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' =
            CPL.eval1 CPL.NamedFunDef{CPL.fun_name = pfun_name, CPL.fun_def = fn} (placeArgs vs vs')
              & (runReaderT ?? eval_env)

      return ((CPL.NamedFunDef pfun_name fn, vs), eval_fn)

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
      fn <- use $ CPL._funCtx . Ctx.at pfun_name . singular _Just
      A.canError fn

    -- split the overall precision in half if fns can error, otherwise use full.
    let eps_alg = min eps_old (if pfuns_may_error then A.splitFailProb eps 2 else eps)

    let n_queries_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let n_queries_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.splitFailProb (eps - eps_alg) (fromIntegral $ length par_funs)

    forM_ (zip3 par_funs n_queries_q n_queries_u) $ \(PartialFun{pfun_name}, n_query_q, n_query_u) -> do
      fn <- use $ CPL._funCtx . Ctx.at pfun_name . singular _Just
      let named_fn = CPL.NamedFunDef pfun_name fn

      -- divide by number of queries to get eps per call
      let eps_fn = A.splitFailProb eps_fns (n_query_q + n_query_u)
      let eps_fn_u = A.unitarySubroutineTVBudget eps_fn

      when (n_query_q > 0) $ void $ A.annEpsQ1 eps_fn named_fn
      when (n_query_u > 0) $ void $ A.annEpsU1 eps_fn_u named_fn

    pure $ A.AnnFailProb eps_alg $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Compilation
-- --------------------------------------------------------------------------------

instance
  ( TypeCheckPrim prim (SizeType prim)
  , CPL.TypingReqs (SizeType prim)
  , UnitaryCompilePrim prim (SizeType prim) (PrecType prim)
  , QuantumCompilePrim prim (SizeType prim) (PrecType prim)
  ) =>
  Compiler.CompileQ (A.AnnFailProb (Primitive prim))
  where
  compileQ (A.AnnFailProb eps (Primitive par_funs prim)) rets = do
    let pfun_names = map pfun_name par_funs
    let bound_args_names = concatMap (catMaybes . pfun_args) par_funs
    bound_args_tys <- forM bound_args_names $ \x -> use $ CPL._typingCtx . Ctx.at x . non' (error $ "invalid arg " ++ x)
    let bound_args = zip bound_args_names bound_args_tys

    mk_ucall <-
      reshape $
        par_funs <&> \PartialFun{pfun_name, pfun_args} xs ->
          QPL.UCallS
            { uproc_id = Compiler.mkUProcName pfun_name
            , dagger = False
            , qargs = placeArgsWithExcess (map (fmap QPL.Arg) pfun_args) xs
            }

    uproc_aux_types <-
      reshape =<< do
        forM par_funs $ \PartialFun{pfun_name} -> do
          let uproc_name = Compiler.mkUProcName pfun_name
          sign <-
            use (Compiler._procSignatures . at uproc_name)
              >>= maybeWithError (printf "could not find uproc `%s` for fun `%s`" uproc_name pfun_name)
          return $ Compiler.aux_tys sign

    mk_call <-
      reshape $
        par_funs <&> \PartialFun{pfun_name, pfun_args} xs ->
          QPL.CallS
            { fun = QPL.FunctionCall $ Compiler.mkQProcName pfun_name
            , meta_params = []
            , args = placeArgsWithExcess (map (fmap QPL.Arg) pfun_args) xs
            }

    mk_meas <-
      reshape $
        par_funs <&> \PartialFun{pfun_name, pfun_args} xs ->
          QPL.CallS
            { fun = QPL.UProcAndMeas $ Compiler.mkUProcName pfun_name
            , meta_params = []
            , args = placeArgsWithExcess (map (fmap QPL.Arg) pfun_args) xs
            }

    prim_ret_types <- forM rets $ \x ->
      use (CPL._typingCtx . Ctx.at x) >>= maybeWithError "missing variable"

    let builder =
          PrimCompileEnv
            { mk_ucall
            , mk_call
            , mk_meas
            , uproc_aux_types
            , prim_ret_types
            }
    let arg_bounder =
          prependBoundArgs
            (map Compiler.mkUProcName pfun_names ++ map Compiler.mkQProcName pfun_names)
            bound_args
    prim_proc_raw <-
      runReaderT (compileQPrim prim eps) builder
        & censor (Compiler._loweredProcs . each %~ arg_bounder)
    let prim_proc = arg_bounder prim_proc_raw
    Compiler.addProc prim_proc

    return $
      QPL.CallS
        { fun = QPL.FunctionCall $ QPL.proc_name prim_proc
        , meta_params = []
        , args = map (QPL.Arg . fst) bound_args ++ map QPL.Arg rets
        }

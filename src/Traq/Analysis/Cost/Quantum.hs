{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Cost.Quantum (
  CostQ (..),
  costQProg,
  ExpCostQ (..),
  expCostQProg,

  -- * Types
  CostModelReqs,
) where

import Control.Monad.Reader (runReader, runReaderT)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad (forAccumM, non', (??))
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default (HasDefault (default_))
import qualified Traq.Data.Probability as Prob

import Traq.Analysis.Cost.Prelude
import Traq.Analysis.Cost.Unitary
import Traq.Analysis.CostModel.Class
import Traq.Analysis.Prelude
import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.Syntax

-- | Havoc Cost w.r.t. quantum compiler
class (CostU ext size prec) => CostQ ext size prec | ext -> size prec where
  costQ ::
    forall ext' cost m.
    ( m ~ CostAnalysisMonad ext'
    , CostQ ext' size prec
    , CostModelReqs size prec cost
    ) =>
    ext ->
    m cost

instance (CostReqs size prec) => CostQ (Core size prec) size prec where
  costQ = \case {}

-- | Expected Cost w.r.t. quantum compiler
class (CostQ ext size prec, Evaluatable ext size prec) => ExpCostQ ext size prec | ext -> size prec where
  expCostQ ::
    forall ext' cost m.
    ( m ~ CostAnalysisMonad ext'
    , ExpCostQ ext' size prec
    , CostModelReqs size prec cost
    ) =>
    ext ->
    ProgramState size ->
    m cost

instance (CostReqs size prec, EvalReqs size prec) => ExpCostQ (Core size prec) size prec where
  expCostQ = \case {}

-- ================================================================================
-- Core Language: Havoc Cost
-- ================================================================================

-- | Havoc Cost w.r.t. quantum compiler
class CostQ1 f where
  costQ1 ::
    forall ext size prec cost m.
    ( m ~ CostAnalysisMonad ext
    , CostQ ext size prec
    , CostModelReqs size prec cost
    ) =>
    f ext ->
    m cost

instance CostQ1 Expr where
  costQ1 BasicExprE{basic_expr} = return $ callExpr Classical basic_expr
  costQ1 RandomSampleE{distr_expr} = return $ callDistrExpr Classical distr_expr
  costQ1 FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    costQ1 $ NamedFunDef fname fn
  costQ1 PrimCallE{prim} = costQ prim
  costQ1 LoopE{loop_body_fun} = do
    fn@FunDef{param_types} <- view $ _funCtx . Ctx.at loop_body_fun . non' (error $ "unable to find function " ++ loop_body_fun)
    body_cost <- costQ1 $ NamedFunDef loop_body_fun fn
    let Fin n_iters = last param_types
    return $ (sizeToPrec n_iters :: prec) Alg..* body_cost

instance CostQ1 Stmt where
  costQ1 ExprS{expr} = costQ1 expr
  costQ1 IfThenElseS{s_true, s_false} = max <$> costQ1 s_true <*> costQ1 s_false
  costQ1 (SeqS ss) = Alg.sum <$> mapM costQ1 ss

instance CostQ1 NamedFunDef where
  -- query an external function
  costQ1 NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} = return $ query Classical fun_name
  -- def: compute using body
  costQ1 NamedFunDef{fun_def = FunDef{mbody = Just FunBody{body_stmt}}} = costQ1 body_stmt

-- ================================================================================
-- Core Language: Expected Cost
-- ================================================================================

class ExpCostQ1 f where
  expCostQ1 ::
    forall ext size prec cost m.
    ( m ~ CostAnalysisMonad ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , ExpCostQ ext size prec
    , CostModelReqs size prec cost
    ) =>
    f ext ->
    EvalArgs f ext ->
    m cost

instance ExpCostQ1 Expr where
  expCostQ1 BasicExprE{basic_expr} _ = return $ callExpr Classical basic_expr
  expCostQ1 RandomSampleE{distr_expr} _ = return $ callDistrExpr Classical distr_expr
  expCostQ1 FunCallE{fname, args} sigma = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    let arg_vals = [sigma ^?! Ctx.at x . non (error $ "could not find var " ++ x) | x <- args]
    expCostQ1 (NamedFunDef fname fn) arg_vals
  expCostQ1 PrimCallE{prim} sigma = expCostQ prim sigma
  expCostQ1 LoopE{initial_args, loop_body_fun} sigma = do
    fn@FunDef{param_types} <- view $ _funCtx . Ctx.at loop_body_fun . non' (error $ "unable to find function " ++ loop_body_fun)
    let init_vals = [sigma ^?! Ctx.at x . non (error $ "could not find var " ++ x) | x <- initial_args]
    let loop_domain = domain (last param_types)

    -- evaluate each iteration
    env <- view _evaluationEnv
    let run_loop_body i args =
          eval1 (NamedFunDef loop_body_fun fn) (args ++ [i])
            & (runReaderT ?? env)

    (_, cs) <- forAccumM (pure init_vals) loop_domain $ \distr i -> do
      let sigma_fn = fmap (\xs -> Ctx.fromList $ zip [show j | j <- [0 :: Int ..]] (xs ++ [i])) distr
      iter_cost <- Prob.expectationA (expCostQ (NamedFunDef loop_body_fun fn)) sigma_fn
      return (distr >>= run_loop_body i, iter_cost)
    return $ Alg.sum cs

-- | TODO unify this as a class instance, after unifying evaluation
expCostQStmt ::
  forall ext size prec cost m.
  ( m ~ CostAnalysisMonad ext
  , ExpCostQ ext size prec
  , CostModelReqs size prec cost
  ) =>
  Stmt ext ->
  ProgramState size ->
  m cost
expCostQStmt ExprS{expr} sigma = expCostQ expr sigma
expCostQStmt IfThenElseS{cond, s_true, s_false} sigma = do
  let b =
        sigma
          ^?! Ctx.at cond
          . non' (error $ "cannot find variable" ++ cond)
          . to valueToBool
  expCostQStmt (if b then s_true else s_false) sigma
expCostQStmt (SeqS ss) sigma = do
  env <- view _evaluationEnv

  let stepS s sigma_s = eval1 s sigma_s & (runReaderT ?? env)

  (_, cs) <- forAccumM (pure sigma) ss $ \distr s -> do
    c <- Prob.expectationA (expCostQStmt s) distr
    return (distr >>= stepS s, c)

  return $ Alg.sum cs

instance ExpCostQ1 NamedFunDef where
  -- query an external function
  expCostQ1
    NamedFunDef
      { fun_name
      , fun_def = FunDef{mbody = Nothing}
      }
    _ =
      return $ query Classical fun_name
  -- def: compute using body
  expCostQ1
    NamedFunDef
      { fun_def = FunDef{mbody = Just FunBody{param_names, body_stmt}}
      }
    args = expCostQStmt body_stmt sigma
     where
      -- bind args to the parameter names
      sigma = Ctx.fromList $ zip param_names args

instance ExpCostQ1 Program where
  expCostQ1 (Program fs) = expCostQ1 $ last fs

-- ================================================================================
-- Entry Points
-- ================================================================================

-- | Havoc (worst-case) quantum cost of the entire program (i.e. last function as entry-point)
costQProg ::
  forall cost ext size prec.
  ( CostQ ext size prec
  , CostModelReqs size prec cost
  ) =>
  Program ext ->
  cost
costQProg (Program []) = Alg.zero
costQProg (Program fs) =
  costQ main_fn & runReader ?? env
 where
  main_fn = last fs
  env =
    default_
      & (_funCtx .~ namedFunsToFunCtx fs)

-- | Expected quantum cost of the entire program (i.e. last function as entry-point)
expCostQProg ::
  forall cost ext size prec.
  ( ExpCostQ ext size prec
  , CostModelReqs size prec cost
  , EvalReqs size prec
  ) =>
  -- | program
  Program ext ->
  -- | args to main
  [Value size] ->
  -- | external function interpretations
  FunInterpCtx size ->
  cost
expCostQProg p args extern_fns = expCostQ1 p args & runReader ?? env
 where
  env =
    default_
      & (_funCtx .~ programToFunCtx p)
      & (_funInterpCtx .~ extern_fns)

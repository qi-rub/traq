{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Monad.State (execStateT)

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

-- ================================================================================
-- Core Language: Havoc Cost
-- ================================================================================

instance (CostQ ext size prec) => CostQ (Expr ext) size prec where
  costQ BasicExprE{basic_expr} = return $ callExpr Classical basic_expr
  costQ RandomSampleE{distr_expr} = return $ callDistrExpr Classical distr_expr
  costQ FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    costQ $ NamedFunDef fname fn
  costQ PrimCallE{prim} = costQ prim
  costQ _ = error "unsupported"

instance (CostQ ext size prec) => CostQ (Stmt ext) size prec where
  costQ ExprS{expr} = costQ expr
  costQ IfThenElseS{s_true, s_false} = max <$> costQ s_true <*> costQ s_false
  costQ (SeqS ss) = Alg.sum <$> mapM costQ ss

instance (CostQ ext size prec) => CostQ (NamedFunDef ext) size prec where
  -- query an external function
  costQ NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} = return $ query Classical fun_name
  -- def: compute using body
  costQ NamedFunDef{fun_def = FunDef{mbody = Just FunBody{body_stmt}}} = costQ body_stmt

instance (CostReqs size prec) => CostQ (Core size prec) size prec where
  costQ = \case {}

-- ================================================================================
-- Core Language: Expected Cost
-- ================================================================================

instance (ExpCostQ ext size prec) => ExpCostQ (Expr ext) size prec where
  expCostQ BasicExprE{basic_expr} _ = return $ callExpr Classical basic_expr
  expCostQ RandomSampleE{distr_expr} _ = return $ callDistrExpr Classical distr_expr
  expCostQ FunCallE{fname, args} sigma = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    let arg_vals = [sigma ^?! Ctx.at x . non (error $ "could not find var " ++ x) | x <- args]
    let sigma_fn = Ctx.fromList $ zip [show i | i <- [0 :: Int ..]] arg_vals
    expCostQ (NamedFunDef fname fn) sigma_fn
  expCostQ PrimCallE{prim} sigma = expCostQ prim sigma
  expCostQ _ _ = error "unsupported"

-- | TODO unify this as a class instance, after unifying evaluation
expCostQStmt ::
  forall ext size prec ext' cost m.
  ( m ~ CostAnalysisMonad ext'
  , ExpCostQ ext size prec
  , ExpCostQ ext' size prec
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

  let stepS s sigma_s =
        s
          & execStmt @_ @_ @_ @ext'
          & (execStateT ?? sigma_s)
          & (runReaderT ?? env)

  (_, cs) <- forAccumM (pure sigma) ss $ \distr s -> do
    c <- Prob.expectationA (expCostQStmt s) distr
    return (distr >>= stepS s, c)

  return $ Alg.sum cs

instance (ExpCostQ ext size prec) => ExpCostQ (NamedFunDef ext) size prec where
  -- query an external function
  expCostQ
    NamedFunDef
      { fun_name
      , fun_def = FunDef{mbody = Nothing}
      }
    _ =
      return $ query Classical fun_name
  -- def: compute using body
  expCostQ
    NamedFunDef
      { fun_def = FunDef{mbody = Just FunBody{param_names, body_stmt}}
      }
    sigma_fn = expCostQStmt body_stmt sigma
     where
      -- extract the arguments in order
      args = Ctx.toAscList sigma_fn & map snd
      -- bind them to the parameter names
      sigma = Ctx.fromList $ zip param_names args

instance (CostReqs size prec, EvalReqs size prec) => ExpCostQ (Core size prec) size prec where
  expCostQ = \case {}

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
expCostQProg (Program []) _ _ = Alg.zero
expCostQProg (Program fs) args extern_fns =
  expCostQ main_fn main_sigma & runReader ?? env
 where
  main_fn = last fs
  main_sigma = Ctx.fromList $ zip [show i | i <- [0 :: Int ..]] args
  env =
    default_
      & (_funCtx .~ namedFunsToFunCtx fs)
      & (_funInterpCtx .~ extern_fns)

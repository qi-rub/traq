{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Cost.Unitary (
  CostU (..),
  costUProg,
  costU1,
) where

import Control.Monad.Reader (runReader)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad (non', (??))
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default (HasDefault (default_))

import Traq.Analysis.Cost.Prelude
import Traq.Analysis.CostModel.Class
import Traq.Analysis.Prelude
import Traq.Prelude
import Traq.ProtoLang.Syntax

-- | Cost w.r.t. unitary compiler
class
  ( CostReqs size prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  ) =>
  CostU ext size prec
    | ext -> size prec
  where
  costU ::
    forall ext' costT m.
    ( m ~ CostAnalysisMonad ext'
    , CostU ext' size prec
    , CostModelReqs size prec costT
    ) =>
    ext ->
    m costT

instance (CostReqs size prec) => CostU (Core size prec) size prec where
  costU = \case {}

-- ================================================================================
-- Core Language
-- ================================================================================

class CostU1 f where
  costU1 ::
    forall ext cost size prec m.
    ( m ~ CostAnalysisMonad ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , CostU ext size prec
    , CostModelReqs size prec cost
    ) =>
    f ext ->
    m cost

instance CostU1 Expr where
  costU1 ::
    forall ext cost size prec m.
    ( m ~ CostAnalysisMonad ext
    , size ~ SizeType ext
    , prec ~ PrecType ext
    , CostU ext size prec
    , CostModelReqs size prec cost
    ) =>
    Expr ext ->
    m cost
  costU1 BasicExprE{basic_expr} = return $ callExpr Unitary basic_expr
  costU1 RandomSampleE{distr_expr} = return $ callDistrExpr Unitary distr_expr
  costU1 FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    costU1 $ NamedFunDef fname fn
  costU1 PrimCallE{prim} = costU prim
  costU1 LoopE{loop_body_fun} = do
    fn@FunDef{param_types} <- view $ _funCtx . Ctx.at loop_body_fun . non' (error $ "unable to find function " ++ loop_body_fun)
    body_cost <- costU1 $ NamedFunDef loop_body_fun fn
    let Fin n_iters = last param_types
    return $ (sizeToPrec n_iters :: prec) Alg..* body_cost

instance CostU1 Stmt where
  costU1 ExprS{expr} = costU1 expr
  costU1 IfThenElseS{s_true, s_false} = do
    cost_t <- costU1 s_true
    cost_f <- costU1 s_false
    return $ cost_t Alg.+ cost_f
  costU1 (SeqS ss) = Alg.sum <$> mapM costU1 ss

instance CostU1 NamedFunDef where
  -- query an external function
  costU1 NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} = return $ query Unitary fun_name
  -- def: compute using body
  costU1 NamedFunDef{fun_def = FunDef{mbody = Just FunBody{body_stmt}}} = costU1 body_stmt

instance CostU1 Program where
  costU1 (Program fs) = costU1 $ last fs

-- ================================================================================
-- Entry Points
-- ================================================================================

-- | Expected quantum cost of the entire program (i.e. last function as entry-point)
costUProg ::
  forall cost ext size prec.
  ( CostU ext size prec
  , CostModelReqs size prec cost
  ) =>
  Program ext ->
  cost
costUProg p = costU1 p & runReader ?? env
 where
  env =
    default_
      & (_funCtx .~ programToFunCtx p)

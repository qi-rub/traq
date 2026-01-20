{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Cost.Unitary (
  CostU (..),
  costUProg,
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
import Traq.Analysis.Prelude (sizeToPrec)
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

-- ================================================================================
-- Core Language
-- ================================================================================

instance (CostU ext size prec) => CostU (Expr ext) size prec where
  costU BasicExprE{basic_expr} = return $ callExpr Unitary basic_expr
  costU RandomSampleE{distr_expr} = return $ callDistrExpr Unitary distr_expr
  costU FunCallE{fname} = do
    fn <- view $ _funCtx . Ctx.at fname . non' (error $ "unable to find function " ++ fname)
    costU $ NamedFunDef fname fn
  costU PrimCallE{prim} = costU prim
  costU LoopE{loop_body_fun} = do
    fn@FunDef{param_types} <- view $ _funCtx . Ctx.at loop_body_fun . non' (error $ "unable to find function " ++ loop_body_fun)
    body_cost <- costU $ NamedFunDef loop_body_fun fn
    let Fin n_iters = last param_types
    return $ (sizeToPrec n_iters :: prec) Alg..* body_cost

instance (CostU ext size prec) => CostU (Stmt ext) size prec where
  costU ExprS{expr} = costU expr
  costU IfThenElseS{s_true, s_false} = do
    cost_t <- costU s_true
    cost_f <- costU s_false
    return $ cost_t Alg.+ cost_f
  costU (SeqS ss) = Alg.sum <$> mapM costU ss

instance (CostU ext size prec) => CostU (NamedFunDef ext) size prec where
  -- query an external function
  costU NamedFunDef{fun_name, fun_def = FunDef{mbody = Nothing}} = return $ query Unitary fun_name
  -- def: compute using body
  costU NamedFunDef{fun_def = FunDef{mbody = Just FunBody{body_stmt}}} = costU body_stmt

instance (CostReqs size prec) => CostU (Core size prec) size prec where
  costU = \case {}

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
costUProg (Program []) = Alg.zero
costUProg (Program fs) =
  costU main_fn & runReader ?? env
 where
  main_fn = last fs
  env =
    default_
      & (_funCtx .~ namedFunsToFunCtx fs)

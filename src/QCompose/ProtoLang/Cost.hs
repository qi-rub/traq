{-# LANGUAGE TypeFamilies #-}

module QCompose.ProtoLang.Cost (
  -- * Abstract Formulas
  QSearchFormulas (..),

  -- * Unitary Cost
  unitaryQueryCost,
  unitaryQueryCostE,
  unitaryQueryCostS,

  -- * Quantum Cost

  -- ** Expected cost
  quantumQueryCost,
  quantumQueryCostE,
  quantumQueryCostS,

  -- ** Quantum Worst case Cost
  quantumMaxQueryCost,
  quantumMaxQueryCostE,
  quantumMaxQueryCostS,

  -- ** Bound on runtime
  quantumQueryCostBound,

  -- * Types and Monad
  OracleName,

  -- ** Static
  StaticCostEnv,
  UnitaryCostCalculator,
  QuantumMaxCostCalculator,
  UnitaryCostablePrimitive (..),

  -- ** Dynamic
  DynamicCostEnv,
  QuantumCostCalculator,

  -- * Analysis
  needsEpsS,
  needsEpsP,
) where

import Control.Monad (filterM)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Foldable (toList)
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import QCompose.Prelude
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- | Computed cost functions (quantum, unitary) of a given set of algorithms implementing quantum search
data QSearchFormulas sizeT costT = QSearchFormulas
  { qSearchExpectedCost :: sizeT -> sizeT -> costT -> costT -- n t eps
  , qSearchWorstCaseCost :: sizeT -> costT -> costT -- n eps
  , qSearchUnitaryCost :: sizeT -> costT -> costT -- n delta
  }

detExtract :: Tree.Tree a -> a
detExtract xs = case toList xs of
  [x] -> x
  _ -> error "unexpected non-determinism"

{-# DEPRECATED unsafeLookupFun "Remove" #-}
unsafeLookupFun :: Ident -> FunCtx primT sizeT -> FunDef primT sizeT
unsafeLookupFun fname funCtx = funCtx ^. Ctx.at fname . singular _Just

-- ================================================================================
-- Unitary Cost
-- ================================================================================

-- | Name of the oracle to compute the number of queries to.
type OracleName = Ident

-- | Environment to compute the unitary cost
type StaticCostEnv primT sizeT costT = (QSearchFormulas sizeT costT, FunCtx primT sizeT, OracleName)

-- | Monad to compute unitary cost.
type UnitaryCostCalculator primT sizeT costT = MyReaderT (StaticCostEnv primT sizeT costT) Maybe

-- | Primitives that support evaluation
class UnitaryCostablePrimitive primT where
  type PrimSizeT primT
  type PrimCostT primT
  unitaryQueryCostPrimitive :: PrimCostT primT -> primT -> UnitaryCostCalculator primsT (PrimSizeT primT) (PrimCostT primT) (PrimCostT primT)

-- | Evaluate the query cost of an expression
unitaryQueryCostE ::
  (Num sizeT, Floating costT) =>
  -- | precision
  costT ->
  -- | expression @E@
  Expr primT sizeT ->
  UnitaryCostCalculator primT sizeT costT costT
unitaryQueryCostE delta FunCallE{fun_kind = FunctionCall fname} = do
  is_oracle_call <- view $ _3 . to (== fname)
  if is_oracle_call
    then return 1.0
    else do
      FunDef{mbody} <- view $ _2 . to (unsafeLookupFun fname)
      case mbody of
        Nothing -> return 0 -- no cost for injected data
        Just FunBody{body_stmt} -> (2 *) <$> unitaryQueryCostS (delta / 2) body_stmt
unitaryQueryCostE delta FunCallE{fun_kind = PrimitiveCallOld c [predicate]}
  | c == "any" || c == "search" = do
      FunDef{param_types} <- view $ _2 . to (unsafeLookupFun predicate)
      let Fin n = param_types & last

      -- split the precision
      let delta_search = delta / 2
      let delta_pred = delta - delta_search

      -- number of predicate queries
      qry_formula <- view $ _1 . to qSearchUnitaryCost
      let qry = qry_formula n delta_search

      -- precision per predicate call
      let delta_per_pred_call = delta_pred / qry

      -- cost of each predicate call
      cost_pred <-
        unitaryQueryCostE delta_per_pred_call $
          FunCallE{fun_kind = FunctionCall predicate, args = undefined}

      return $ qry * cost_pred

-- zero-cost expressions
unitaryQueryCostE _ VarE{} = return 0
unitaryQueryCostE _ ConstE{} = return 0
unitaryQueryCostE _ UnOpE{} = return 0
unitaryQueryCostE _ BinOpE{} = return 0
unitaryQueryCostE _ TernaryE{} = return 0
unitaryQueryCostE _ FunCallE{} = error "invalid syntax"

-- Evaluate the query cost of a statement
unitaryQueryCostS ::
  forall primT sizeT costT.
  (Num sizeT, Floating costT) =>
  -- | precision (l2-norm)
  costT ->
  -- | statement @S@
  Stmt primT sizeT ->
  UnitaryCostCalculator primT sizeT costT costT
unitaryQueryCostS delta ExprS{expr} = unitaryQueryCostE delta expr
unitaryQueryCostS delta IfThenElseS{s_true, s_false} = do
  cost_true <- unitaryQueryCostS delta s_true
  cost_false <- unitaryQueryCostS delta s_false
  return $ cost_true + cost_false
unitaryQueryCostS delta (SeqS [s]) = unitaryQueryCostS delta s
unitaryQueryCostS delta (SeqS (s : ss)) = do
  cost_s <- unitaryQueryCostS (delta / 2) s
  cost_rest <- unitaryQueryCostS (delta / 2) (SeqS ss)
  return $ cost_s + cost_rest
unitaryQueryCostS _ _ = error "invalid syntax"

unitaryQueryCost ::
  forall primT sizeT costT.
  (Num sizeT, Floating costT) =>
  -- | Qry formulas
  QSearchFormulas sizeT costT ->
  -- | precision (l2-norm)
  costT ->
  -- | program @P@
  Program primT sizeT ->
  -- | oracle name @O@
  OracleName ->
  costT
unitaryQueryCost algs delta Program{funCtx, stmt} oracle_name =
  let env = (algs, funCtx, oracle_name)
   in unitaryQueryCostS delta stmt `runMyReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Max Cost
-- ================================================================================

-- Environment to compute the max quantum cost (input independent)
type QuantumMaxCostCalculator primT sizeT costT = MyReaderT (StaticCostEnv primT sizeT costT) Maybe

quantumMaxQueryCostE ::
  (Num sizeT, Ord costT, Floating costT) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | statement @S@
  Expr primT sizeT ->
  QuantumMaxCostCalculator primT sizeT costT costT
quantumMaxQueryCostE eps FunCallE{fun_kind = FunctionCall fname} = do
  is_oracle_call <- view $ _3 . to (== fname)
  if is_oracle_call
    then return 1
    else do
      FunDef{mbody} <- view $ _2 . to (unsafeLookupFun fname)
      case mbody of
        Nothing -> return 0 -- no cost for injected data
        Just FunBody{body_stmt} -> quantumMaxQueryCostS eps body_stmt

-- -- known cost formulas
quantumMaxQueryCostE eps FunCallE{fun_kind = PrimitiveCallOld c [predicate]}
  | c == "any" || c == "search" = do
      FunDef{param_types} <- view $ _2 . to (unsafeLookupFun predicate)

      let Fin n = last param_types

      worst_case_formula <- view $ _1 . to qSearchWorstCaseCost

      let eps_s = eps / 2
      let n_pred_calls = worst_case_formula n eps_s
      let eps_per_pred_call = (eps - eps_s) / n_pred_calls

      let delta_per_pred_call = eps_per_pred_call / 2
      pred_unitary_cost <-
        unitaryQueryCostE delta_per_pred_call $
          FunCallE{fun_kind = FunctionCall predicate, args = undefined}

      return $ n_pred_calls * pred_unitary_cost

-- -- zero-cost expressions
quantumMaxQueryCostE _ VarE{} = return 0
quantumMaxQueryCostE _ ConstE{} = return 0
quantumMaxQueryCostE _ UnOpE{} = return 0
quantumMaxQueryCostE _ BinOpE{} = return 0
quantumMaxQueryCostE _ TernaryE{} = return 0
-- -- unsupported
quantumMaxQueryCostE _ FunCallE{} = error "invalid syntax"

quantumMaxQueryCostS ::
  (Num sizeT, Ord costT, Floating costT) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | statement @S@
  Stmt primT sizeT ->
  QuantumMaxCostCalculator primT sizeT costT costT
quantumMaxQueryCostS eps ExprS{expr} = quantumMaxQueryCostE eps expr
quantumMaxQueryCostS eps IfThenElseS{s_true, s_false} =
  max <$> quantumMaxQueryCostS eps s_true <*> quantumMaxQueryCostS eps s_false
quantumMaxQueryCostS eps (SeqS [s]) = quantumMaxQueryCostS eps s
quantumMaxQueryCostS eps (SeqS (s : ss)) = do
  cost_s <- quantumMaxQueryCostS (eps / 2) s
  cost_ss <- quantumMaxQueryCostS (eps / 2) (SeqS ss)
  return $ cost_s + cost_ss
quantumMaxQueryCostS _ _ = error "INVALID"

quantumMaxQueryCost ::
  forall primT sizeT costT.
  (Num sizeT, Ord costT, Floating costT) =>
  -- | Qry formulas
  QSearchFormulas sizeT costT ->
  -- | failure probability `eps`
  costT ->
  -- | program `P`
  Program primT sizeT ->
  -- | oracle @O@
  OracleName ->
  costT
quantumMaxQueryCost algs a_eps Program{funCtx, stmt} oracle_name =
  let env = (algs, funCtx, oracle_name)
   in quantumMaxQueryCostS a_eps stmt `runMyReaderT` env ^. singular _Just

-- ================================================================================
-- Quantum Cost
-- ================================================================================

-- Environment to compute the quantum cost (input dependent)
type DynamicCostEnv primT sizeT costT = (QSearchFormulas sizeT costT, FunCtx primT sizeT, FunInterpCtx, OracleName)

type QuantumCostCalculator primT sizeT costT = MyReaderT (DynamicCostEnv primT sizeT costT) Maybe

quantumQueryCostE ::
  (Floating costT) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | state \( \sigma \)
  ProgramState ->
  -- | statement @S@
  Expr primT SizeT ->
  QuantumCostCalculator primT SizeT costT costT
quantumQueryCostE eps sigma FunCallE{fun_kind = FunctionCall f, args} = do
  is_oracle <- (f ==) <$> view _4
  if is_oracle
    then return 1
    else
      view (_2 . to (unsafeLookupFun f) . to mbody) >>= \case
        Nothing -> return 0 -- no cost for injected data
        Just FunBody{param_names, body_stmt} -> do
          let vs = args & map (\x -> sigma ^. Ctx.at x . singular _Just)
          -- bind the arguments to the parameter names
          let omega = Ctx.fromList $ zip param_names vs
          quantumQueryCostS eps omega body_stmt

-- -- known cost formulas
quantumQueryCostE eps sigma FunCallE{fun_kind = PrimitiveCallOld c [predicate], args}
  | c == "any" || c == "search" = do
      let vs = args & map (\x -> sigma ^. Ctx.at x . singular _Just)

      predDef@FunDef{param_types} <- view $ _2 . to (unsafeLookupFun predicate)
      let typ_x = last param_types

      let space = range typ_x
      sols <- do
        env <- (,) <$> view _2 <*> view _3
        -- TODO this is too convoluted...
        return $
          (`runMyReaderT` env) $
            (`filterM` space)
              ( \v -> do
                  result <- evalFun (vs ++ [v]) predDef
                  let [b] = result
                  return $ b /= 0
              )

      let n = length space
      let t = minimum $ fmap length sols

      let eps_search = eps / 2
      let eps_pred = eps - eps_search

      exp_case_formula <- view $ _1 . to qSearchExpectedCost
      let n_pred_calls = exp_case_formula n t eps_search

      worst_case_formula <- view $ _1 . to qSearchWorstCaseCost
      let q_worst = worst_case_formula n eps_search
      let eps_per_pred_call = eps_pred / q_worst
      let delta_per_pred_call = eps_per_pred_call / 2

      pred_unitary_cost <-
        magnify extractUEnv $
          unitaryQueryCostE delta_per_pred_call FunCallE{fun_kind = FunctionCall predicate, args = undefined}
      return $ n_pred_calls * pred_unitary_cost

-- -- zero-cost expressions
quantumQueryCostE _ _ VarE{} = return 0
quantumQueryCostE _ _ ConstE{} = return 0
quantumQueryCostE _ _ UnOpE{} = return 0
quantumQueryCostE _ _ BinOpE{} = return 0
quantumQueryCostE _ _ TernaryE{} = return 0
-- -- unsupported
quantumQueryCostE _ _ FunCallE{} = error "invalid syntax"

-- TODO use proper argument names
extractUEnv :: Lens' (DynamicCostEnv primT sizeT costT) (StaticCostEnv primT sizeT costT)
extractUEnv focus (a, b, c, d) = (\(a', b', d') -> (a', b', c, d')) <$> focus (a, b, d)

quantumQueryCostS ::
  (Floating costT) =>
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | state \( \sigma \)
  ProgramState ->
  -- | statement @S@
  Stmt primT SizeT ->
  QuantumCostCalculator primT SizeT costT costT
quantumQueryCostS eps sigma ExprS{expr} = quantumQueryCostE eps sigma expr
quantumQueryCostS eps sigma IfThenElseS{cond, s_true, s_false} =
  let s = if sigma ^. Ctx.at cond /= Just 0 then s_true else s_false
   in quantumQueryCostS eps sigma s
quantumQueryCostS eps sigma (SeqS [s]) = quantumQueryCostS eps sigma s
quantumQueryCostS eps sigma (SeqS (s : ss)) = do
  cost_s <- quantumQueryCostS (eps / 2) sigma s

  funCtx <- view _2
  interpCtx <- view _3
  let sigma' = detExtract $ runProgram Program{funCtx, stmt = s} interpCtx sigma

  cost_ss <- quantumQueryCostS (eps / 2) sigma' (SeqS ss)
  return $ cost_s + cost_ss
quantumQueryCostS _ _ _ = error "INVALID"

quantumQueryCost ::
  forall primT costT.
  (Floating costT) =>
  -- | Qry formulas
  QSearchFormulas SizeT costT ->
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primT SizeT ->
  -- | oracle @O@
  OracleName ->
  -- | data injections
  FunInterpCtx ->
  -- | state \( \sigma \)
  ProgramState ->
  costT
quantumQueryCost algs a_eps Program{funCtx, stmt} oracle_name interpCtx sigma =
  let env = (algs, funCtx, interpCtx, oracle_name)
   in quantumQueryCostS a_eps sigma stmt `runMyReaderT` env ^. singular _Just

-- | The bound on the true expected runtime which fails with probability <= \eps.
quantumQueryCostBound ::
  forall primT costT.
  (Ord costT, Floating costT) =>
  -- | Qry formulas
  QSearchFormulas SizeT costT ->
  -- | failure probability \( \varepsilon \)
  costT ->
  -- | program @P@
  Program primT SizeT ->
  -- | oracle @O@
  OracleName ->
  -- | data injections
  FunInterpCtx ->
  -- | state \( \sigma \)
  ProgramState ->
  costT
quantumQueryCostBound algs a_eps p oracle_name interp_ctx sigma =
  (1 - a_eps) * quantumQueryCost algs a_eps p oracle_name interp_ctx sigma
    + a_eps * quantumMaxQueryCost algs a_eps p oracle_name

-- | Compute if a statement can fail and therefore needs a failure probability to implement
needsEpsS :: Stmt primT sizeT -> MyReaderT (FunCtx primT sizeT) Identity Bool
needsEpsS IfThenElseS{s_true, s_false} = (||) <$> needsEpsS s_true <*> needsEpsS s_false
needsEpsS (SeqS ss) = or <$> traverse needsEpsS ss
needsEpsS ExprS{expr = FunCallE{fun_kind = PrimitiveCallOld _ _}} = return True
needsEpsS ExprS{expr = FunCallE{fun_kind = FunctionCall f}} = do
  view (to (unsafeLookupFun f) . to mbody) >>= \case
    Just FunBody{body_stmt} -> needsEpsS body_stmt
    Nothing -> return False
needsEpsS _ = return False

-- | Compute if a program can fail and therefore needs a failure probability to implement
needsEpsP :: Program primT sizeT -> Bool
needsEpsP Program{funCtx, stmt} = runIdentity $ runMyReaderT (needsEpsS stmt) funCtx

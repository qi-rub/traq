module QCompose.ProtoLang.Cost (
  Complexity,
  QSearchFormulas (..),
  unitaryQueryCost,
  quantumQueryCost,
) where

import Control.Monad (filterM)
import Control.Monad.State (execStateT)
import Data.Foldable (toList)
import qualified Data.Map as M
import Lens.Micro

import QCompose.Prelude
import QCompose.Utils.Tree

import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- | Computed cost functions (quantum, unitary) of a given set of algorithms implementing quantum search
data QSearchFormulas = QSearchFormulas
  { qSearchExpectedCost :: SizeT -> SizeT -> FailProb -> Complexity -- n t eps
  , qSearchWorstCaseCost :: SizeT -> FailProb -> Complexity -- n eps
  , qSearchUnitaryCost :: SizeT -> Precision -> Complexity -- n delta
  }

detExtract :: Tree a -> a
detExtract xs = case toList xs of
  [x] -> x
  _ -> error "unexpected non-determinism"

unitaryQueryCost ::
  -- | Qry formulas
  QSearchFormulas ->
  -- | precision (l2-norm)
  Precision ->
  -- | program `P`
  Program SizeT ->
  Complexity
unitaryQueryCost algs d Program{funCtx, stmt} = cost d stmt
 where
  unsafeLookupFun :: Ident -> FunDef SizeT
  unsafeLookupFun fname = detExtract $ lookupFun fname funCtx

  costE :: Precision -> Expr SizeT -> Complexity
  costE _ FunCallE{fun_kind = OracleCall} = 1
  -- TODO properly handle the funCtx
  costE delta FunCallE{fun_kind = FunctionCall fname} =
    let FunDef{body} = unsafeLookupFun fname
     in cost delta body
  costE delta FunCallE{fun_kind = PrimitiveCall c, args = (predicate : args)}
    | c == Contains || c == Search =
        2 * qry * cost_pred
   where
    FunDef{param_binds} = unsafeLookupFun predicate
    Fin n = param_binds & last & snd

    delta_search = delta / 2
    qry = qSearchUnitaryCost algs n delta_search
    delta_per_pred_call = (delta - delta_search) / (2 * qry)
    cost_pred =
      costE delta_per_pred_call $
        FunCallE
          { fun_kind = FunctionCall predicate
          , args = args
          }
  -- zero-cost expressions
  costE _ VarE{} = 0
  costE _ ConstE{} = 0
  costE _ UnOpE{} = 0
  costE _ BinOpE{} = 0
  costE _ FunCallE{} = error "invalid syntax"

  cost :: Precision -> Stmt SizeT -> Complexity
  cost delta ExprS{expr} = costE delta expr
  cost delta IfThenElseS{s_true, s_false} = max (cost delta s_true) (cost delta s_false)
  cost delta (SeqS [s]) = cost delta s
  cost delta (SeqS (s : ss)) = cost (delta / 2) s + cost (delta / 2) (SeqS ss)
  cost _ _ = error "invalid syntax"

quantumQueryCost ::
  -- | Qry formulas
  QSearchFormulas ->
  -- | failure probability `eps`
  FailProb ->
  -- | program `P`
  Program SizeT ->
  -- | oracle `O`
  OracleInterp ->
  -- | state `sigma`
  ProgramState ->
  Complexity
quantumQueryCost algs a_eps Program{funCtx, stmt} oracleF = cost a_eps stmt
 where
  unsafeLookupFun :: Ident -> FunDef SizeT
  unsafeLookupFun fname = head $ lookupFun fname funCtx

  get :: ProgramState -> Ident -> Value
  get st x = st M.! x

  costE :: FailProb -> Expr SizeT -> ProgramState -> Complexity
  costE _ FunCallE{fun_kind = OracleCall} _ = 1
  costE eps FunCallE{fun_kind = FunctionCall f, args} sigma = cost eps body omega
   where
    vs = map (get sigma) args
    FunDef _ fn_args _ body = unsafeLookupFun f
    omega = M.fromList $ zip (fst <$> fn_args) vs

  -- known cost formulas
  costE eps FunCallE{fun_kind = PrimitiveCall c, args = (predicate : args)} sigma
    | c == Contains || c == Search = n_pred_calls * pred_unitary_cost
   where
    vs = map (get sigma) args
    funDef@(FunDef _ fn_args _ body) = unsafeLookupFun predicate
    typ_x = snd $ last fn_args

    space = range typ_x
    sols = (`filterM` space) $ \v -> do
      result <- evalFun funCtx oracleF (vs ++ [v]) funDef
      let [b] = result
      return $ b /= 0

    n = length space
    t = minimum $ fmap length sols

    n_pred_calls = qSearchExpectedCost algs n t (eps / 2)

    q_worst = qSearchWorstCaseCost algs n (eps / 2)
    eps_per_pred_call = (eps / 2) / q_worst
    delta_per_pred_call = eps_per_pred_call / 2

    pred_unitary_cost = unitaryQueryCost algs delta_per_pred_call Program{funCtx, stmt = body}

  -- zero-cost expressions
  costE _ VarE{} _ = 0
  costE _ ConstE{} _ = 0
  costE _ UnOpE{} _ = 0
  costE _ BinOpE{} _ = 0
  -- unsupported
  costE _ FunCallE{} _ = error "invalid syntax"

  cost :: FailProb -> Stmt SizeT -> ProgramState -> Complexity
  cost eps ExprS{expr} sigma = costE eps expr sigma
  cost eps IfThenElseS{cond, s_true, s_false} sigma =
    let s = if get sigma cond /= 0 then s_true else s_false
     in cost eps s sigma
  cost eps (SeqS [s]) sigma = cost eps s sigma
  cost eps (SeqS (s : ss)) sigma = cost (eps / 2) s sigma + cost (eps / 2) (SeqS ss) sigma'
   where
    runner = execProgram Program{funCtx, stmt = s} oracleF
    sigma' = detExtract $ execStateT runner sigma
  -- all other statements
  cost _ _ _ = error "INVALID"

module QCompose.ProtoLang.Cost where

import Control.Monad (filterM)
import Control.Monad.State (execStateT)
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- | Value type for representing the query complexity.
type Complexity = Float

-- | Computed cost functions (quantum, unitary) of a given set of algorithms implementing quantum search
data QSearchFormulas = QSearchFormulas
  { qSearchExpectedCost :: SizeT -> SizeT -> FailProb -> Complexity -- n t eps
  , qSearchWorstCaseCost :: SizeT -> FailProb -> Complexity -- n eps
  , qSearchUnitaryCost :: SizeT -> Precision -> Complexity -- n delta
  }

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
    unsafeLookupFun = either error id . lookupFun funCtx

    cost :: Precision -> Stmt SizeT -> Complexity
    cost _ FunCallS{fun_kind = OracleCall} = 1
    -- TODO properly handle the funCtx
    cost delta FunCallS{fun_kind = FunctionCall fname} =
      let FunDef{body} = unsafeLookupFun fname
       in cost delta body
    cost delta (SeqS [s]) = cost delta s
    cost delta (SeqS (s : ss)) = cost (delta / 2) s + cost (delta / 2) (SeqS ss)
    cost delta FunCallS{fun_kind = SubroutineCall c, args = (predicate : args), rets = [b]}
      | c == Contains || c == Search =
          2 * qry * cost_pred
      where
        -- arg_ty :: VarType SizeT
        FunDef{params} = unsafeLookupFun predicate
        Fin n = snd (last params)

        delta_search = delta / 2
        qry = qSearchUnitaryCost algs n delta_search
        cost_pred =
          cost ((delta - delta_search) / (2 * qry)) $
            FunCallS
              { fun_kind = FunctionCall predicate
              , args = args
              , rets = [b]
              }

    -- unknown subroutine
    cost _ FunCallS{fun_kind = SubroutineCall _} = error "unknown subroutine"
    -- all other statements
    cost _ _ = 0

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
quantumQueryCost algs a_eps Program{funCtx, stmt} oracleF = cost stmt a_eps
  where
    get :: ProgramState -> Ident -> Value
    get st x = st M.! x

    cost :: Stmt SizeT -> FailProb -> ProgramState -> Complexity
    cost IfThenElseS{..} eps sigma =
      let s = if get sigma cond /= 0 then s_true else s_false
       in cost s eps sigma
    cost (SeqS [s]) eps sigma = cost s eps sigma
    cost (SeqS (s : ss)) eps sigma = cost s (eps / 2) sigma + cost (SeqS ss) (eps / 2) sigma'
      where
        runner = evalProgram Program{funCtx, stmt = s} oracleF
        sigma' = either error id $ execStateT runner sigma
    cost FunCallS{fun_kind = OracleCall} _ _ = 1
    cost FunCallS{fun_kind = FunctionCall f, ..} eps sigma = cost body eps omega
      where
        vs = map (get sigma) args
        FunDef _ fn_args _ body = either error id $ lookupFun funCtx f
        omega = M.fromList $ zip (fst <$> fn_args) vs

    -- known cost formulas
    cost FunCallS{fun_kind = SubroutineCall c, args = (predicate : args)} eps sigma
      | c == Contains || c == Search = either error id $ do
          let vs = map (get sigma) args
          funDef@(FunDef _ fn_args _ body) <- lookupFun funCtx predicate
          let typ_x = snd $ last fn_args

          let space = range typ_x
          sols <- (`filterM` space) $ \v -> do
            result <- evalFun funCtx oracleF (vs ++ [v]) funDef
            let [b] = result
            return $ b /= 0

          let n = length space
          let t = length sols

          let n_pred_calls = qSearchExpectedCost algs n t (eps / 2)

          let q_worst = qSearchWorstCaseCost algs n (eps / 2)
          let eps_per_pred_call = (eps / 2) / q_worst

          let pred_unitary_cost = unitaryQueryCost algs eps_per_pred_call Program{funCtx, stmt = body}

          return $ n_pred_calls * pred_unitary_cost

    -- unknown subroutines
    cost FunCallS{fun_kind = SubroutineCall _} _ _ = error "unknown subroutine"
    -- all other statements
    cost _ _ _ = 0

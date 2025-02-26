module QCompose.ProtoLang.Cost where

import Control.Monad.State (execStateT)
import Data.Either (fromRight)
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- | Value type for representing the query complexity.
type Complexity = Float

-- | CostMetric takes: Oracle interpretation, Program, eps (fail prob), sigma (input state)
type CostMetric = OracleInterp -> Program SizeT -> FailProb -> ProgramState -> Complexity

data CostType = Quantum | Unitary deriving (Eq, Show, Read)

-- | Computed cost functions (quantum, unitary) of a given set of algorithms implementing quantum search
data QSearchFormulas = QSearchFormulas
  { qSearchExpectedCost :: SizeT -> SizeT -> FailProb -> Complexity -- n t eps
  , qSearchWorstCaseCost :: SizeT -> FailProb -> Complexity -- n eps
  , qSearchUnitaryCost :: SizeT -> FailProb -> Complexity -- n eps
  }

{- | Cost formulas for quantum search from the paper
 [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
cadeEtAlFormulas :: QSearchFormulas
cadeEtAlFormulas = QSearchFormulas eqsearch eqsearch_worst zalka
  where
    eqsearch_worst :: SizeT -> FailProb -> Complexity
    eqsearch_worst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

    f :: SizeT -> SizeT -> Complexity
    f n t
      | 4 * t < n = 2.0344
      | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

    eqsearch :: SizeT -> SizeT -> FailProb -> Complexity
    eqsearch n t eps
      | t == 0 = eqsearch_worst n eps
      | otherwise = f n t * (1 + 1 / (1 - term))
      where
        term = f n t / (9.2 * sqrt (fromIntegral n))

    zalka :: SizeT -> FailProb -> Complexity
    zalka n eps = 5 * fromIntegral log_fac + pi * sqrt (fromIntegral (n * log_fac))
      where
        log_fac :: SizeT
        log_fac = ceiling (log (1 / eps) / (2 * log (4 / 3)))

quantumQueryCost :: CostType -> QSearchFormulas -> CostMetric
quantumQueryCost flag algs oracleF Program{funCtx, stmt} = cost stmt
  where
    get :: ProgramState -> Ident -> Value
    get st x = st M.! x

    cost :: Stmt SizeT -> FailProb -> ProgramState -> Complexity
    cost IfThenElseS{..} eps sigma =
      case flag of
        Unitary -> max (cost s_true eps sigma) (cost s_false eps sigma)
        Quantum ->
          let s = if get sigma cond /= 0 then s_true else s_false
           in cost s eps sigma
    cost (SeqS [s]) eps sigma = cost s eps sigma
    cost (SeqS (s : ss)) eps sigma = cost s (eps / 2) sigma + cost (SeqS ss) (eps / 2) sigma'
      where
        runner = evalProgram Program{funCtx, stmt = s} oracleF
        sigma' = fromRight undefined $ execStateT runner sigma
    cost FunCallS{fun_kind = OracleCall} _ _ = 1
    cost FunCallS{fun_kind = FunctionCall f, ..} eps sigma = cost body eps omega
      where
        vs = map (get sigma) args
        FunDef _ fn_args _ body = fromRight undefined $ lookupFun funCtx f
        omega = M.fromList $ zip (fst <$> fn_args) vs

    -- known cost formulas
    cost FunCallS{fun_kind = SubroutineCall Contains, ..} eps sigma = n_pred_calls * max_pred_unitary_cost
      where
        vs = map (get sigma) (tail args)
        funDef@(FunDef _ fn_args _ body) = fromRight undefined $ lookupFun funCtx (head args)
        typ_x = snd $ last fn_args

        check :: Value -> Bool
        check v = b /= 0
          where
            result = fromRight undefined $ evalFun funCtx oracleF (vs ++ [v]) funDef
            [b] = result

        n = length $ range typ_x
        t = length $ filter check (range typ_x)

        n_pred_calls = case flag of
          Quantum -> qSearchExpectedCost algs n t (eps / 2)
          Unitary -> qSearchUnitaryCost algs n (eps / 2)

        q_worst = case flag of
          Quantum -> qSearchWorstCaseCost algs n (eps / 2)
          Unitary -> qSearchUnitaryCost algs n (eps / 2)
        eps_per_pred_call = (eps / 2) / q_worst

        pred_unitary_cost :: Value -> Complexity
        pred_unitary_cost v =
          quantumQueryCost
            Unitary
            algs
            oracleF
            Program{funCtx, stmt = body}
            eps_per_pred_call
            omega
          where
            omega = M.fromList $ zip (map fst fn_args) (vs ++ [v])

        max_pred_unitary_cost = maximum $ pred_unitary_cost <$> range typ_x
    cost _ _ _ = 0

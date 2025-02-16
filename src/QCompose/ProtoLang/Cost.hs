module QCompose.ProtoLang.Cost where

import qualified Data.Map as M
import Numeric.Extra (intToFloat)
import QCompose.Basic
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- value type for representing the query complexity
type Complexity = Float

-- cost
type CostMetric = FunCtx -> OracleInterp -> Stmt -> FailProb -> State -> Complexity

-- Functions, Oracle interpretation, S (program), eps (fail prob), sigma (input state)

data CostType = Quantum | Unitary deriving (Eq, Show, Read)

-- computed cost functions of a given set of algorithms (quantum, unitary)
data QSearchFormulas = QSearchFormulas
  { qSearchExpectedCost :: SizeT -> SizeT -> FailProb -> Complexity -- n t eps
  , qSearchWorstCaseCost :: SizeT -> FailProb -> Complexity -- n eps
  , qSearchUnitaryCost :: SizeT -> FailProb -> Complexity -- n eps
  }

-- example
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
    zalka n eps = 5 * err + pi * sqrt (fromIntegral n * err)
      where
        err :: FailProb
        err = intToFloat $ ceiling (log (1 / eps) / (2 * log (4 / 3)))

quantumQueryCost :: CostType -> QSearchFormulas -> CostMetric
quantumQueryCost flag algs funCtx@FunCtx{funs} oracleF = cost
  where
    get :: State -> Ident -> Value
    get st x = st M.! x

    cost :: Stmt -> FailProb -> State -> Complexity
    cost SAssign{} _ _ = 0
    cost SConst{} _ _ = 0
    cost SUnOp{} _ _ = 0
    cost SBinOp{} _ _ = 0
    cost SOracle{} _ _ = 1
    cost (SIfTE _ s_t s_f) eps sigma = max (cost s_t eps sigma) (cost s_f eps sigma)
    cost (SSeq s_1 s_2) eps sigma = cost s_1 (eps / 2) sigma + cost s_2 (eps / 2) sigma'
      where
        sigma' = evalStmt funCtx oracleF sigma s_1
    cost (SFunCall _ f args) eps sigma = cost body eps omega
      where
        vs = map (get sigma) args
        FunDef _ fn_args _ body = funs M.! f
        omega = M.fromList $ zip (fst <$> fn_args) vs

    -- known cost formulas
    cost (SContains _ f xs) eps sigma = n_pred_calls * max_pred_unitary_cost
      where
        vs = map (get sigma) xs
        FunDef _ fn_args _ body = funs M.! f
        typ_x = snd $ last fn_args

        check :: Value -> Bool
        check v = b /= 0
          where
            result = evalFun funCtx oracleF (vs ++ [v]) f
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
            funCtx
            oracleF
            body
            eps_per_pred_call
            omega
          where
            omega = M.fromList $ zip (map fst fn_args) (vs ++ [v])

        max_pred_unitary_cost = maximum $ pred_unitary_cost <$> range typ_x
    cost SSearch{} _ _ = error "cost for search not supported, use contains for now."

quantumQueryCostOfFun :: CostType -> QSearchFormulas -> FunCtx -> OracleInterp -> [Value] -> FailProb -> Ident -> Complexity
quantumQueryCostOfFun flag algs funCtx@FunCtx{funs} oracle in_values eps f = cost
  where
    FunDef _ fn_args _ body = funs M.! f
    param_names = map fst fn_args
    sigma = M.fromList $ zip param_names in_values

    cost = quantumQueryCost flag algs funCtx oracle body eps sigma

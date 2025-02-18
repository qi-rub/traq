module QCompose.ProtoLang.Cost where

import Data.Either (fromRight)
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- value type for representing the query complexity
type Complexity = Float

-- cost
type CostMetric = FunCtx SizeT -> OracleInterp -> Stmt SizeT -> FailProb -> State -> Complexity

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
    zalka n eps = 5 * fromIntegral log_fac + pi * sqrt (fromIntegral (n * log_fac))
      where
        log_fac :: SizeT
        log_fac = ceiling (log (1 / eps) / (2 * log (4 / 3)))

quantumQueryCost :: CostType -> QSearchFormulas -> CostMetric
quantumQueryCost flag algs funCtx oracleF = cost
  where
    get :: State -> Ident -> Value
    get st x = st M.! x

    cost :: Stmt SizeT -> FailProb -> State -> Complexity
    cost SAssign{} _ _ = 0
    cost SConst{} _ _ = 0
    cost SUnOp{} _ _ = 0
    cost SBinOp{} _ _ = 0
    cost SOracle{} _ _ = 1
    cost (SIfTE _ s_t s_f) eps sigma = max (cost s_t eps sigma) (cost s_f eps sigma)
    cost (SSeq []) _ _ = 0
    cost (SSeq [s]) eps sigma = cost s eps sigma
    cost (SSeq (s : ss)) eps sigma = cost s (eps / 2) sigma + cost (SSeq ss) (eps / 2) sigma'
      where
        sigma' = evalProgram Program{funCtx, stmt = s} oracleF sigma
    cost (SFunCall _ f args) eps sigma = cost body eps omega
      where
        vs = map (get sigma) args
        FunDef _ fn_args _ body = fromRight undefined $ lookupFun funCtx f
        omega = M.fromList $ zip (fst <$> fn_args) vs

    -- known cost formulas
    cost (SContains _ f xs) eps sigma = n_pred_calls * max_pred_unitary_cost
      where
        vs = map (get sigma) xs
        funDef@(FunDef _ fn_args _ body) = fromRight undefined $ lookupFun funCtx f
        typ_x = snd $ last fn_args

        check :: Value -> Bool
        check v = b /= 0
          where
            result = evalFun funCtx oracleF (vs ++ [v]) funDef
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

quantumQueryCostOfFun :: CostType -> QSearchFormulas -> FunCtx SizeT -> OracleInterp -> [Value] -> FailProb -> Ident -> Complexity
quantumQueryCostOfFun flag algs funCtx oracle in_values eps f = cost
  where
    FunDef _ fn_args _ body = fromRight undefined $ lookupFun funCtx f
    param_names = map fst fn_args
    sigma = M.fromList $ zip param_names in_values

    cost = quantumQueryCost flag algs funCtx oracle body eps sigma

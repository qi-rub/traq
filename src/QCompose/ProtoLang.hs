module QCompose.ProtoLang where

import qualified Data.Map as M
import Extra (intToFloat)

-- proto-search language
newtype VarType = Fin Int -- Fin<N>
  deriving (Eq, Show, Read)

type Ident = String

data UnOp = PNot
  deriving (Eq, Show, Read)

data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Stmt
  = SAssign Ident Ident -- x... <- y...
  | SConst Ident Int VarType -- x <- v :: T
  | SUnOp Ident UnOp Ident -- x <- op y
  | SBinOp Ident BinOp Ident Ident -- x <- y `binop` z
  | SOracle [Ident] [Ident] -- x... <- Oracle(y...)
  | SFunCall [Ident] Ident [Ident] -- x... <- f(y...)
  | SIfTE Ident Stmt Stmt -- if x then E_1 else E_0
  | SSeq Stmt Stmt -- S_1; S_2
  | SSearch Ident Ident Ident [Ident] -- x, ok <- search(f, [x_1, ... x_{k-1}])
  | SContains Ident Ident [Ident] -- ok <- contains(f, [x_1, ... x_{k-1}])
  deriving (Eq, Show, Read)

data FunDef = FunDef [(Ident, VarType)] [(Ident, VarType)] Stmt -- args@(x_i : T_i) ret_vals@(x'_j : T'_j) body
  deriving (Eq, Show, Read)

type FunCtx = M.Map Ident FunDef

-- evaluation
range :: VarType -> [Int]
range (Fin n) = [0 .. n - 1]

type State = M.Map Ident Int

type Oracle = [Int] -> [Int]

evalStmt :: FunCtx -> Oracle -> State -> Stmt -> State
evalStmt fns oracle = \st s -> foldr (uncurry M.insert) st (eval_ st s)
  where
    get :: State -> Ident -> Int
    get st x = st M.! x

    eval_ :: State -> Stmt -> [(Ident, Int)]

    eval_ st (SAssign x y) = return (y, get st x)
    eval_ _ (SConst x v _) = return (x, v)
    eval_ st (SUnOp res op x) =
      let vx = get st x
       in let vres = case op of PNot -> if vx == 0 then 1 else 0
           in return (res, vres)
    eval_ st (SBinOp res op x y) =
      let vx = get st x
          vy = get st y
       in let vres =
                case op of
                  PAdd -> vx + vy
                  PLeq -> if vx <= vy then 1 else 0
                  PAnd -> if vx /= 0 && vy /= 0 then 1 else 0
           in return (res, vres)
    eval_ st (SOracle outs args) = zip outs (oracle $ map (get st) args)
    eval_ st (SFunCall outs f args) = zip outs ret_vals
      where
        arg_vals = map (get st) args
        ret_vals = evalFun fns oracle arg_vals f
    eval_ st (SIfTE x s_t s_f) = eval_ st s
      where
        b = get st x /= 0
        s = if b then s_t else s_f
    eval_ st (SSeq s_1 s_2) =
      let st' = evalStmt fns oracle st s_1 in eval_ st' s_2
    eval_ st (SContains ok f xs) = return (ok, if any check (range typ_x) then 1 else 0)
      where
        vs = map (get st) xs
        FunDef fn_args _ _ = fns M.! f
        typ_x = snd $ last fn_args

        check :: Int -> Bool
        check v = b /= 0
          where
            [b] = evalFun fns oracle (vs ++ [v]) f
    eval_ _ (SSearch {}) = error "non-deterministic"

evalFun :: FunCtx -> Oracle -> [Int] -> Ident -> [Int]
evalFun fns oracle in_values f = ret_vals
  where
    FunDef fn_args fn_rets body = fns M.! f
    param_names = map fst fn_args
    sigma = M.fromList $ zip param_names in_values
    sigma' = evalStmt fns oracle sigma body

    out_names = map fst fn_rets
    ret_vals = map (sigma' M.!) out_names

-- cost
type CostMetric = FunCtx -> Oracle -> Stmt -> Float -> State -> Float
-- Functions, Oracle interpretation, S (program), eps (fail prob), sigma (input state)

data CostType = Quantum | Unitary deriving (Eq, Show, Read)

-- computed cost functions of a given set of algorithms (quantum, unitary)
data QSearchFormulas = QSearchFormulas
  { qSearchExpectedCost :: Int -> Int -> Float -> Float, -- n t eps
    qSearchWorstCaseCost :: Int -> Float -> Float, -- n eps
    qSearchUnitaryCost :: Int -> Float -> Float -- n eps
  }

-- example
cadeEtAlFormulas :: QSearchFormulas
cadeEtAlFormulas = QSearchFormulas eqsearch eqsearch_worst zalka
  where
    eqsearch_worst :: Int -> Float -> Float
    eqsearch_worst n eps = 9.2 * log (1 / eps) * sqrt (fromIntegral n)

    f :: Int -> Int -> Float
    f n t
      | 4 * t < n = 2.0344
      | otherwise = 3.1 * sqrt (fromIntegral n / fromIntegral t)

    eqsearch :: Int -> Int -> Float -> Float
    eqsearch n t eps 
      | t == 0 = eqsearch_worst n eps
      | otherwise = f n t * (1 + 1 / (1 - term))
        where
          term = f n t / (9.2 * sqrt (fromIntegral n))

    zalka :: Int -> Float -> Float
    zalka n eps = 5 * err + pi * sqrt (fromIntegral n * err)
      where
        err :: Float
        err = intToFloat $ ceiling (log (1 / eps) / (2 * log (4 / 3)))

quantumQueryCost :: CostType -> QSearchFormulas -> CostMetric
quantumQueryCost flag algs fns oracle = cost
  where
    get :: State -> Ident -> Int
    get st x = st M.! x

    cost :: Stmt -> Float -> State -> Float
    cost (SAssign {}) _ _ = 0
    cost (SConst {}) _ _ = 0
    cost (SUnOp {}) _ _ = 0
    cost (SBinOp {}) _ _ = 0
    cost (SOracle {}) _ _ = 1
    cost (SIfTE x s_t s_f) eps sigma = max (cost s_t eps sigma) (cost s_f eps sigma)
    cost (SSeq s_1 s_2) eps sigma = cost s_1 (eps / 2) sigma + cost s_2 (eps / 2) sigma'
      where
        sigma' = evalStmt fns oracle sigma s_1
    cost (SFunCall _ f args) eps sigma = cost body eps omega
      where
        vs = map (get sigma) args
        FunDef fn_args _ body = fns M.! f
        omega = M.fromList $ zip (fst <$> fn_args) vs

    -- known cost formulas
    cost (SContains _ f xs) eps sigma = n_pred_calls * max_pred_unitary_cost
      where
        vs = map (get sigma) xs
        FunDef fn_args _ body = fns M.! f
        typ_x = snd $ last fn_args

        check :: Int -> Bool
        check v = b /= 0
          where
            [b] = evalFun fns oracle (vs ++ [v]) f

        n = length $ range typ_x
        t = length $ filter check (range typ_x)

        n_pred_calls = case flag of
          Quantum -> qSearchExpectedCost algs n t (eps / 2)
          Unitary -> qSearchUnitaryCost algs n (eps / 2)

        q_worst = case flag of
          Quantum -> qSearchWorstCaseCost algs n (eps / 2)
          Unitary -> qSearchUnitaryCost algs n (eps / 2)
        eps_per_pred_call = (eps / 2) / q_worst

        pred_unitary_cost :: Int -> Float
        pred_unitary_cost v =
          quantumQueryCost
            Unitary
            algs
            fns
            oracle
            body
            eps_per_pred_call
            omega
          where
            omega = M.fromList $ zip (map fst fn_args) (vs ++ [v])

        max_pred_unitary_cost = maximum $ pred_unitary_cost <$> range typ_x
    cost (SSearch {}) _ _ = error "cost for search not supported, use contains for now."

quantumQueryCostOfFun :: CostType -> QSearchFormulas -> FunCtx -> Oracle -> [Int] -> Float -> Ident -> Float
quantumQueryCostOfFun flag algs fns oracle in_values eps f = cost
  where
    FunDef fn_args _ body = fns M.! f
    param_names = map fst fn_args
    sigma = M.fromList $ zip param_names in_values

    cost = quantumQueryCost flag algs fns oracle body eps sigma


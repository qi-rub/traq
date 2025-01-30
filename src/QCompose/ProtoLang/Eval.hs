module QCompose.ProtoLang.Eval where

import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

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
    eval_ _ (SSearch{}) = error "non-deterministic"

evalFun :: FunCtx -> Oracle -> [Int] -> Ident -> [Int]
evalFun fns oracle in_values f = ret_vals
  where
    FunDef fn_args fn_rets body = fns M.! f
    param_names = map fst fn_args
    sigma = M.fromList $ zip param_names in_values
    sigma' = evalStmt fns oracle sigma body

    out_names = map fst fn_rets
    ret_vals = map (sigma' M.!) out_names

module QCompose.ProtoLang.Eval where

import Data.Map ((!))
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

-- evaluation
range :: VarType -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

type State = M.Map Ident Value

type OracleInterp = [Value] -> [Value]

evalStmt :: FunCtx -> OracleInterp -> State -> Stmt -> State
evalStmt funCtx@FunCtx{..} oracleF = \st s -> foldr (uncurry M.insert) st (eval_ st s)
  where
    get :: State -> Ident -> Value
    get st x = st ! x

    eval_ :: State -> Stmt -> [(Ident, Value)]

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
    eval_ st (SOracle outs args) = zip outs (oracleF $ map (get st) args)
    eval_ st (SFunCall outs f args) = zip outs ret_vals
      where
        arg_vals = map (get st) args
        ret_vals = evalFun funCtx oracleF arg_vals f
    eval_ st (SIfTE x s_t s_f) = eval_ st s
      where
        b = get st x /= 0
        s = if b then s_t else s_f
    eval_ st (SSeq s_1 s_2) =
      let st' = evalStmt funCtx oracleF st s_1 in eval_ st' s_2
    eval_ st (SContains ok f xs) = return (ok, if any check (range typ_x) then 1 else 0)
      where
        vs = map (get st) xs
        FunDef fn_args _ _ = funs ! f
        typ_x = snd $ last fn_args

        check :: Value -> Bool
        check v = b /= 0
          where
            [b] = evalFun funCtx oracleF (vs ++ [v]) f
    eval_ _ (SSearch{}) = error "non-deterministic"

evalFun :: FunCtx -> OracleInterp -> [Value] -> Ident -> [Value]
evalFun funCtx@FunCtx{..} oracleF in_values f = ret_vals
  where
    FunDef fn_args fn_rets body = funs ! f

    param_names = map fst fn_args
    out_names = map fst fn_rets

    sigma = M.fromList $ zip param_names in_values
    sigma' = evalStmt funCtx oracleF sigma body

    ret_vals = map (sigma' !) out_names

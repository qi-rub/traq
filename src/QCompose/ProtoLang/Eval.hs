module QCompose.ProtoLang.Eval where

import Data.Either (fromRight)
import Data.Map ((!))
import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

-- evaluation
range :: VarType SizeT -> [Value]
range (Fin n) = [0 .. fromIntegral n - 1]

type State = M.Map Ident Value

type OracleInterp = [Value] -> [Value]

evalProgram :: Program SizeT -> OracleInterp -> State -> State
evalProgram prog@Program{funCtx = funCtx@FunCtx{..}, stmt} oracleF = \st -> foldr (uncurry M.insert) st (eval_ st stmt)
  where
    get :: State -> Ident -> Value
    get st x = st ! x

    eval_ :: State -> Stmt SizeT -> [(Ident, Value)]

    eval_ st (AssignS x y) = return (y, get st x)
    eval_ _ (ConstS x v _) = return (x, v)
    eval_ st (UnOpS res op x) =
      let vx = get st x
       in let vres = case op of NotOp -> if vx == 0 then 1 else 0
           in return (res, vres)
    eval_ st (BinOpS res op x y) =
      let vx = get st x
          vy = get st y
       in let vres =
                case op of
                  AddOp -> vx + vy
                  LEqOp -> if vx <= vy then 1 else 0
                  AndOp -> if vx /= 0 && vy /= 0 then 1 else 0
           in return (res, vres)
    eval_ st (OracleS outs args) = zip outs (oracleF $ map (get st) args)
    eval_ st (FunCallS outs f args) = zip outs ret_vals
      where
        arg_vals = map (get st) args
        funDef = fromRight undefined $ lookupFun funCtx f
        ret_vals = evalFun funCtx oracleF arg_vals funDef
    eval_ st (IfThenElseS x s_t s_f) = eval_ st s
      where
        b = get st x /= 0
        s = if b then s_t else s_f
    eval_ _ (SeqS []) = []
    eval_ st (SeqS [s]) = eval_ st s
    eval_ st (SeqS (s : ss)) =
      let st' = evalProgram prog{stmt = s} oracleF st in eval_ st' (SeqS ss)
    eval_ st (ContainsS ok f xs) = return (ok, if any check (range typ_x) then 1 else 0)
      where
        vs = map (get st) xs
        funDef@(FunDef _ fn_args _ _) = fromRight undefined $ lookupFun funCtx f
        typ_x = snd $ last fn_args

        check :: Value -> Bool
        check v = b /= 0
          where
            [b] = evalFun funCtx oracleF (vs ++ [v]) funDef
    eval_ _ SearchS{} = error "non-deterministic"

evalFun :: FunCtx SizeT -> OracleInterp -> [Value] -> FunDef SizeT -> [Value]
evalFun funCtx oracleF in_values funDef = ret_vals
  where
    FunDef _ fn_args fn_rets body = funDef

    param_names = map fst fn_args
    out_names = map fst fn_rets

    sigma = M.fromList $ zip param_names in_values
    sigma' = evalProgram Program{funCtx, stmt = body} oracleF sigma

    ret_vals = map (sigma' !) out_names

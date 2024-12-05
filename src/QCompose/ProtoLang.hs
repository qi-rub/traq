module QCompose.ProtoLang where

import qualified Data.Map as M

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
    get st x =
      case M.lookup x st of
        Just v -> v
        _ -> error "invalid input"

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
    eval_ st (SFunCall outs f args) = zip outs fn_out_vals
      where
        arg_vals = map (get st) args
        Just (FunDef fn_args fn_ret fn_body) = M.lookup fns f
        fn_in_st = M.fromList $ zip (map fst fn_args) arg_vals
        fn_out_vals = map snd $ eval_ fn_in_st fn_body
    eval_ st (SIfTE x s_t s_f) = eval_ st s
      where
        b = get st x /= 0
        s = if b then s_t else s_f
    eval_ st (SSeq s_1 s_2) =
      let st' = evalStmt fns oracle st s_1 in eval_ st' s_2
    eval_ st (SContains ok f xs) = return (ok, any ())
      where
        vs = map (get st) xs
        Just (FunDef fn_args fn_ret fn_body) = M.lookup fns f

    eval_ _ (SSearch {}) = error "non-deterministic"


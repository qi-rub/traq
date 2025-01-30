module QCompose.ProtoLang.Syntax where

import qualified Data.Map as M
import qualified Data.Set as S
import QCompose.Basic

-- proto-search language
newtype VarType = Fin Int -- Fin<N>
  deriving (Eq, Show, Read)

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

-- syntactic properties
stmtVars :: Stmt -> S.Set Ident
stmtVars (SAssign x x') = S.fromList [x, x']
stmtVars (SConst x _ _) = S.fromList [x]
stmtVars (SUnOp x _ y) = S.fromList [x, y]
stmtVars (SBinOp x _ lhs rhs) = S.fromList [x, lhs, rhs]
stmtVars (SOracle xs ys) = S.fromList $ xs <> ys
stmtVars (SFunCall xs _ ys) = S.fromList $ xs <> ys
stmtVars (SIfTE b s_t s_f) =
  let vars = S.union (stmtVars s_t) (stmtVars s_f)
   in S.insert b vars
stmtVars (SSeq s_1 s_2) = S.union (stmtVars s_1) (stmtVars s_2)
stmtVars (SSearch x ok _ args) = S.fromList (x : ok : args)
stmtVars (SContains ok _ args) = S.fromList (ok : args)

funVars :: FunDef -> S.Set Ident
funVars (FunDef params rets body) = S.union (S.fromList (map fst params <> map fst rets)) (stmtVars body)

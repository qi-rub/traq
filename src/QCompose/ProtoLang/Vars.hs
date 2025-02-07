module QCompose.ProtoLang.Vars where

import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

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
funVars FunDef{..} =
  S.union (S.fromList (map fst params <> map fst rets)) (stmtVars body)

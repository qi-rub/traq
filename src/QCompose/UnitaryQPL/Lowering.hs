module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt -> UQPLStmt
lowerU _ (SAssign x x') = UUnitary [x', x] XorInto
lowerU _ (SConst x v _) = UUnitary [x] (XorConst v)
lowerU _ (SUnOp res PNot arg) = UUnitary [arg, res] C0_X
lowerU _ (SBinOp res PAdd lhs rhs) = UUnitary [lhs, rhs, res] AddInto
lowerU _ (SBinOp res PLeq lhs rhs) = UUnitary [lhs, rhs, res] LEqInto
lowerU _ (SBinOp res PAnd lhs rhs) = UUnitary [lhs, rhs, res] Toffoli
lowerU _ (SOracle res args) = UOracle (args <> res)
-- lowerU delta (SFunCall res fname args) = UOracle (args <> res)
lowerU delta (SSeq s1 s2) = USeq (lowerU (delta / 2) s1) (lowerU (delta / 2) s2)
lowerU delta (SContains ok fname args) = undefined
lowerU _ _ = error "unsupported syntax"

lowerUDef :: Precision -> FunDef -> UProcDef
lowerUDef delta fn = error "not implemented"

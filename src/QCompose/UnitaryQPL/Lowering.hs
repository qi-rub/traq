module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt -> UQPLStmt
lowerU _ SAssign{..} = UUnitary [arg, ret] XorInto
lowerU _ SConst{..} = UUnitary [ret] (XorConst val)
lowerU _ SUnOp{..} = UUnitary [arg, ret] C0_X
lowerU _ SBinOp{..} = UUnitary [lhs, rhs, ret] opUnitary
  where
    opUnitary = case bin_op of
      PAdd -> AddInto
      PLeq -> LEqInto
      PAnd -> Toffoli
lowerU _ SOracle{..} = UOracle (args <> rets)
lowerU delta (SSeq s1 s2) = USeq (lowerU (delta / 2) s1) (lowerU (delta / 2) s2)
lowerU _ _ = error "unsupported syntax"

lowerUDef :: Precision -> FunDef -> UProcDef
lowerUDef _ _ = error "not implemented"

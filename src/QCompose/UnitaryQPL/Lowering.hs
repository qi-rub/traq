module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt SizeT -> UQPLStmt
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
lowerU _ (SSeq []) = USkip
lowerU delta (SSeq [s]) = lowerU delta s
lowerU delta (SSeq (s : ss)) = USeq (lowerU (delta / 2) s) (lowerU (delta / 2) (SSeq ss))
lowerU _ _ = error "unsupported syntax"

lowerUDef :: Precision -> FunDef SizeT -> UProcDef
lowerUDef _ _ = error "not implemented"

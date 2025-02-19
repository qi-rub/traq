module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt SizeT -> UQPLStmt
lowerU _ AssignS{..} = UUnitary [arg, ret] XorInto
lowerU _ ConstS{..} = UUnitary [ret] (XorConst val)
lowerU _ UnOpS{..} = UUnitary [arg, ret] C0_X
lowerU _ BinOpS{..} = UUnitary [lhs, rhs, ret] opUnitary
  where
    opUnitary = case bin_op of
      AddOp -> AddInto
      LEqOp -> LEqInto
      AndOp -> Toffoli
lowerU _ OracleS{..} = UUnitary (args <> rets) Oracle
lowerU _ (SeqS []) = USkip
lowerU delta (SeqS [s]) = lowerU delta s
lowerU delta (SeqS (s : ss)) = USeq (lowerU (delta / 2) s) (lowerU (delta / 2) (SeqS ss))
lowerU _ _ = error "unsupported syntax"

lowerUDef :: Precision -> FunDef SizeT -> UProcDef
lowerUDef _ _ = error "not implemented"

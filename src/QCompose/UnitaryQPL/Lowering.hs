module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt SizeT -> UQPLStmt
lowerU _ AssignS{..} = UnitaryU [arg, ret] XorInto
lowerU _ ConstS{..} = UnitaryU [ret] (XorConst val)
lowerU _ UnOpS{..} = UnitaryU [arg, ret] C0_X
lowerU _ BinOpS{..} = UnitaryU [lhs, rhs, ret] opUnitary
  where
    opUnitary = case bin_op of
      AddOp -> AddInto
      LEqOp -> LEqInto
      AndOp -> Toffoli
lowerU _ (SeqS []) = SkipU
lowerU delta (SeqS [s]) = lowerU delta s
lowerU delta (SeqS (s : ss)) = SeqU (lowerU (delta / 2) s) (lowerU (delta / 2) (SeqS ss))
lowerU _ _ = error "unsupported syntax"

lowerDefU :: Precision -> FunDef SizeT -> UQPLProcDef
lowerDefU _ _ = error "not implemented"

lowerProgramU :: Precision -> Program SizeT -> UQPLProgram
lowerProgramU eps Program{..} = undefined

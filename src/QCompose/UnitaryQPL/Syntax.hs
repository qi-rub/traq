module QCompose.UnitaryQPL.Syntax where

import QCompose.Basic
import QCompose.ProtoLang.Syntax (VarType)

data Unitary
  = XorInto
  | XorConst Int
  | AddInto
  | LEqInto
  | Toffoli
  | CNOT
  | C0_X
  deriving (Eq, Show, Read)

data UQPLStmt
  = USkip
  | UUnitary [Ident] Unitary -- q... *= U
  | UOracle [Ident] -- Oracle(q...)
  | UCall Ident [Ident] -- call F(q...)
  | UCallDagger Ident [Ident] -- call F(q...)
  | USeq UQPLStmt UQPLStmt -- P_1 ; P_2
  deriving (Eq, Show, Read)

data UProcDef = UProcDef [(Ident, VarType)] UQPLStmt

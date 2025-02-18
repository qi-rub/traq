module QCompose.UnitaryQPL.Syntax where

import QCompose.Basic
import QCompose.ProtoLang.Syntax (VarType)
import QCompose.Utils.Printing

data Unitary
  = XorInto
  | XorConst Value
  | AddInto
  | LEqInto
  | Toffoli
  | CNOT
  | C0_X
  | Oracle
  deriving (Eq, Show, Read)

data UQPLStmt
  = USkip
  | UUnitary [Ident] Unitary -- q... *= U
  | UCall Ident [Ident] -- call F(q...)
  | UCallDagger Ident [Ident] -- call F(q...)
  | USeq UQPLStmt UQPLStmt -- P_1 ; P_2
  deriving (Eq, Show, Read)

data UProcDef = UProcDef Ident [(Ident, VarType SizeT)] UQPLStmt

instance ToCodeString UQPLStmt where
  toCodeLines USkip = ["skip"]
  toCodeLines (UUnitary q u) = [qc <> " := " <> show u <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (UCall f q) = ["call " <> f <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (UCallDagger f q) = ["call " <> f <> "^\\dagger" <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (USeq p1 p2) = toCodeLines p1 <> toCodeLines p2

instance ToCodeString UProcDef where
  toCodeLines (UProcDef name params body) =
    ["proc " <> name <> "(" <> plist <> ") do"]
      <> indent (toCodeLines body)
    where
      plist = commaList (map showParam params)

      showParam (ident, ty) = ident <> " : " <> show ty

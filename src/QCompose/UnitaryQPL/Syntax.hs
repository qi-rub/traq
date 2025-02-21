module QCompose.UnitaryQPL.Syntax where

import QCompose.Basic
import QCompose.ProtoLang.Syntax (OracleDecl, VarType)
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
  = SkipU
  | UnitaryU [Ident] Unitary -- q... *= U
  | CallU Ident [Ident] -- call F(q...)
  | CallDaggerU Ident [Ident] -- call F(q...)
  | SeqU UQPLStmt UQPLStmt -- P_1 ; P_2
  deriving (Eq, Show, Read)

data UQPLProcDef = UQPLProcDef
  { procName :: Ident
  , procArgs :: [(Ident, VarType SizeT)]
  , procBody :: UQPLStmt
  }
  deriving (Eq, Show, Read)

data UQPLProgram = UQPLProgram
  { oracleU :: OracleDecl SizeT
  , procs :: [UQPLProcDef]
  , stmtU :: UQPLStmt
  }
  deriving (Eq, Show, Read)

instance ToCodeString UQPLStmt where
  toCodeLines SkipU = ["skip"]
  toCodeLines (UnitaryU q u) = [qc <> " := " <> show u <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (CallU f q) = ["call " <> f <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (CallDaggerU f q) = ["call " <> f <> "^\\dagger" <> "(" <> qc <> ")"]
    where
      qc = commaList q
  toCodeLines (SeqU p1 p2) = toCodeLines p1 <> toCodeLines p2

instance ToCodeString UQPLProcDef where
  toCodeLines (UQPLProcDef name params body) =
    ["proc " <> name <> "(" <> plist <> ") do"]
      <> indent (toCodeLines body)
    where
      plist = commaList (map showParam params)

      showParam (ident, ty) = ident <> " : " <> show ty

instance ToCodeString UQPLProgram where
  toCodeLines UQPLProgram{..} = toCodeString oracleU : map toCodeString procs <> [toCodeString stmtU]

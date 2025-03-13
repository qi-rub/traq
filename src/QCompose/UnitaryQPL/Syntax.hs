module QCompose.UnitaryQPL.Syntax where

import QCompose.Basic
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

data ReversibleFun a
  = ConstF {ty :: P.VarType a, val :: Integer} -- () -> val
  | NotF {ty :: P.VarType a} -- x -> ~x
  | IdF {ty :: P.VarType a} -- x -> x
  | AddF {ty :: P.VarType a} -- x, y -> x + y
  | LEqF {ty :: P.VarType a} -- x, y -> x <= y
  deriving (Eq, Show, Read)

data Unitary a
  = Toffoli
  | CNOT
  | Oracle
  | RevEmbed (ReversibleFun a)
  deriving (Eq, Show, Read)

data Stmt a
  = SkipU
  | UnitaryU {args :: [Ident], unitary :: Unitary a} -- q... *= U
  | CallU {proc_name :: Ident, args :: [Ident]} -- call F(q...)
  | CallDaggerU {proc_name :: Ident, args :: [Ident]} -- call F(q...)
  | SeqU [Stmt a] -- W1; W2; ...
  deriving (Eq, Show, Read)

data ProcDef a = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, P.VarType SizeT)]
  , proc_body :: Stmt a
  }
  deriving (Eq, Show, Read)

data Program a = Program
  { oracleU :: P.OracleDecl a
  , procs :: [ProcDef a]
  , stmtU :: Stmt a
  }
  deriving (Eq, Show, Read)

showTypedIdent :: (Show a) => (String, P.VarType a) -> String
showTypedIdent (ident, ty) = ident <> " : " <> toCodeString ty

showTypedValue :: (Show a, Show v) => (v, P.VarType a) -> String
showTypedValue (v, ty) = show v <> " : " <> toCodeString ty

instance (Show a) => ToCodeString (ReversibleFun a) where
  toCodeString ConstF{val, ty} = "() => " <> showTypedValue (val, ty)
  toCodeString NotF{ty} = showTypedIdent ("x", ty) <> " => ~x"
  toCodeString IdF{ty} = showTypedIdent ("x", ty) <> " => x"
  toCodeString AddF{ty} = showTypedIdent ("x", ty) <> ", " <> showTypedIdent ("y", ty) <> " => x+y"
  toCodeString LEqF{ty} = showTypedIdent ("x", ty) <> ", " <> showTypedIdent ("y", ty) <> " => x≤y"

instance (Show a) => ToCodeString (Unitary a) where
  toCodeString (RevEmbed f) = "Utry[" <> toCodeString f <> "]"
  toCodeString u = show u

instance (Show a) => ToCodeString (Stmt a) where
  toCodeLines SkipU = ["skip"]
  toCodeLines UnitaryU{args, unitary} = [qc <> " := " <> toCodeString unitary <> "(" <> qc <> ")"]
    where
      qc = commaList args
  toCodeLines CallU{proc_name, args} = ["call " <> proc_name <> "(" <> qc <> ")"]
    where
      qc = commaList args
  toCodeLines CallDaggerU{proc_name, args} = ["call " <> proc_name <> "†" <> "(" <> qc <> ")"]
    where
      qc = commaList args
  toCodeLines (SeqU ps) = concatMap toCodeLines ps

instance (Show a) => ToCodeString (ProcDef a) where
  toCodeLines ProcDef{proc_name, proc_params, proc_body} =
    ["proc " <> proc_name <> "(" <> plist <> ") do"]
      <> indent (toCodeLines proc_body)
    where
      plist = commaList (map showTypedIdent proc_params)

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{oracleU, procs, stmtU} = toCodeString oracleU : map toCodeString procs <> [toCodeString stmtU]

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
  | CallU {proc_id :: Ident, args :: [Ident]} -- call F(q...)
  | CallDaggerU {proc_id :: Ident, args :: [Ident]} -- call F(q...)
  | SeqU [Stmt a] -- W1; W2; ...
  deriving (Eq, Show, Read)

data ProcDef a = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, P.VarType SizeT)]
  , proc_body :: Stmt a
  }
  deriving (Eq, Show, Read)

data Program a = Program
  { oracle_decl :: P.OracleDecl a
  , proc_defs :: [ProcDef a]
  , stmt :: Stmt a
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
  toCodeLines CallU{proc_id, args} = ["call " <> proc_id <> "(" <> qc <> ")"]
    where
      qc = commaList args
  toCodeLines CallDaggerU{proc_id, args} = ["call " <> proc_id <> "†" <> "(" <> qc <> ")"]
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
  toCodeLines Program{oracle_decl, proc_defs, stmt} = toCodeString oracle_decl : map toCodeString proc_defs <> [toCodeString stmt]

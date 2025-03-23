module QCompose.UnitaryQPL.Syntax where

import Text.Printf (printf)

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

data ClassicalFun a
  = ConstF {ty :: P.VarType a, val :: Integer} -- () -> val
  | NotF {ty :: P.VarType a} -- x -> ~x
  | IdF {ty :: P.VarType a} -- x -> x
  | AddF {ty :: P.VarType a} -- x, y -> x + y
  | LEqF {ty :: P.VarType a} -- x, y -> x <= y
  deriving (Eq, Show, Read)

data BlackBox a
  = BlackBox Ident
  | QSearchBB {pred_name :: Ident, n_pred_calls :: Float}
  deriving (Eq, Show, Read)

data Unitary a
  = Toffoli
  | CNOT
  | XGate
  | HGate
  | Oracle
  | -- | maps \( |0\rangle \) to \( \frac1{\sqrt{|\Sigma_T|}} \sum_{x \in \Sigma_T} |x\rangle \)
    Unif (P.VarType a)
  | RevEmbedU (ClassicalFun a)
  | BlackBoxU (BlackBox a)
  deriving (Eq, Show, Read)

data Stmt a
  = SkipS
  | UnitaryS {args :: [Ident], unitary :: Unitary a} -- q... *= U
  | CallS {proc_id :: Ident, dagger :: Bool, args :: [Ident]} -- call F(q...)
  | SeqS [Stmt a] -- W1; W2; ...
  deriving (Eq, Show, Read)

data ProcDef a = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, P.VarType a)]
  , proc_body :: Stmt a
  }
  deriving (Eq, Show, Read)

data OracleDecl a = OracleDecl
  { param_types :: [P.VarType a]
  }
  deriving (Eq, Show, Read)

data Program a = Program
  { oracle_decl :: OracleDecl a
  , proc_defs :: [ProcDef a]
  , stmt :: Stmt a
  }
  deriving (Eq, Show, Read)

showTypedIdent :: (Show a) => (String, P.VarType a) -> String
showTypedIdent (ident, ty) = ident <> " : " <> toCodeString ty

showTypedValue :: (Show a, Show v) => (v, P.VarType a) -> String
showTypedValue (v, ty) = show v <> " : " <> toCodeString ty

instance (Show a) => ToCodeString (ClassicalFun a) where
  toCodeString ConstF{val, ty} = "() => " <> showTypedValue (val, ty)
  toCodeString NotF{ty} = showTypedIdent ("x", ty) <> " => ~x"
  toCodeString IdF{ty} = showTypedIdent ("x", ty) <> " => x"
  toCodeString AddF{ty} = showTypedIdent ("x", ty) <> ", " <> showTypedIdent ("y", ty) <> " => x+y"
  toCodeString LEqF{ty} = showTypedIdent ("x", ty) <> ", " <> showTypedIdent ("y", ty) <> " => x≤y"

instance (Show a) => ToCodeString (Unitary a) where
  toCodeString (RevEmbedU f) = "RevEmbed[" <> toCodeString f <> "]"
  toCodeString (Unif ty) = "Unif[" <> toCodeString ty <> "]"
  toCodeString u = show u

instance (Show a) => ToCodeString (Stmt a) where
  toCodeLines SkipS = ["skip;"]
  toCodeLines UnitaryS{args, unitary} = [qc <> " *= " <> toCodeString unitary <> ";"]
   where
    qc = commaList args
  toCodeLines CallS{proc_id, dagger, args} = ["call " <> proc_id <> dg <> "(" <> qc <> ");"]
   where
    qc = commaList args
    dg = if dagger then "†" else ""
  toCodeLines (SeqS ps) = concatMap toCodeLines ps

instance (Show a) => ToCodeString (ProcDef a) where
  toCodeLines ProcDef{proc_name, proc_params, proc_body} =
    ["uproc " <> proc_name <> "(" <> plist <> ") do"]
      <> indent (toCodeLines proc_body)
      <> ["end"]
   where
    plist = commaList $ map showTypedIdent proc_params

instance (Show a) => ToCodeString (OracleDecl a) where
  toCodeString OracleDecl{param_types} =
    printf "uproc Oracle(%s)" plist
   where
    plist = commaList $ map toCodeString param_types

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{oracle_decl, proc_defs, stmt} = toCodeString oracle_decl : "" : map toCodeString proc_defs <> [toCodeString stmt]

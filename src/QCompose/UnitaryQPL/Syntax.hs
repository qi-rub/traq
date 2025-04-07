module QCompose.UnitaryQPL.Syntax where

import Text.Printf (printf)

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

data ClassicalFun sizeT
  = ConstF {ty :: P.VarType sizeT, val :: Integer} -- () -> val
  | NotF {ty :: P.VarType sizeT} -- x -> ~x
  | IdF {ty :: P.VarType sizeT} -- x -> x
  | AddF {ty :: P.VarType sizeT} -- x, y -> x + y
  | LEqF {ty :: P.VarType sizeT} -- x, y -> x <= y
  deriving (Eq, Show, Read)

data BlackBox costT
  = BlackBox Ident
  | QSearchBB {pred_name :: Ident, n_pred_calls :: costT}
  deriving (Eq, Show, Read)

data Unitary sizeT costT
  = Toffoli
  | CNOT
  | XGate
  | HGate
  | Oracle
  | -- | maps \( |0\rangle \) to \( \frac1{\sqrt{|\Sigma_T|}} \sum_{x \in \Sigma_T} |x\rangle \)
    Unif (P.VarType sizeT)
  | UnifDagger (P.VarType sizeT)
  | RevEmbedU (ClassicalFun sizeT)
  | BlackBoxU (BlackBox costT)
  | Controlled (Unitary sizeT costT)
  deriving (Eq, Show, Read)

class HasDagger a where
  adjoint :: a -> a

instance HasDagger (Unitary sizeT costT) where
  adjoint (BlackBoxU _) = error "cannot compute adjoint of blackbox"
  adjoint (Unif ty) = UnifDagger ty
  adjoint (UnifDagger ty) = Unif ty
  adjoint Toffoli = Toffoli
  adjoint CNOT = CNOT
  adjoint HGate = HGate
  adjoint XGate = XGate
  adjoint Oracle = Oracle
  adjoint u@(RevEmbedU _) = u
  adjoint (Controlled u) = Controlled (adjoint u)

data Stmt sizeT costT
  = SkipS
  | UnitaryS {args :: [Ident], unitary :: Unitary sizeT costT} -- q... *= U
  | CallS {proc_id :: Ident, dagger :: Bool, args :: [Ident]} -- call F(q...)
  | SeqS [Stmt sizeT costT] -- W1; W2; ...
  deriving (Eq, Show, Read)

instance HasDagger (Stmt sizeT costT) where
  adjoint SkipS = SkipS
  adjoint s@CallS{dagger} = s{dagger = not dagger}
  adjoint (SeqS ss) = SeqS . reverse $ map adjoint ss
  adjoint s@UnitaryS{unitary} = s{unitary = adjoint unitary}

data ParamTag = ParamInp | ParamOut | ParamAux | ParamUnk deriving (Eq, Show, Read, Enum)

data ProcDef sizeT costT = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, ParamTag, P.VarType sizeT)]
  , proc_body :: Stmt sizeT costT
  }
  deriving (Eq, Show, Read)

newtype OracleDecl a = OracleDecl
  { param_types :: [P.VarType a]
  }
  deriving (Eq, Show, Read)

data Program sizeT costT = Program
  { oracle_decl :: OracleDecl sizeT
  , proc_defs :: [ProcDef sizeT costT]
  , stmt :: Stmt sizeT costT
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

instance (Show a, Show b) => ToCodeString (Unitary a b) where
  toCodeString (RevEmbedU f) = "RevEmbed[" <> toCodeString f <> "]"
  toCodeString (Unif ty) = "Unif[" <> toCodeString ty <> "]"
  toCodeString u = show u

instance (Show a, Show b) => ToCodeString (Stmt a b) where
  toCodeLines SkipS = ["skip;"]
  toCodeLines UnitaryS{args, unitary} = [qc <> " *= " <> toCodeString unitary <> ";"]
   where
    qc = commaList args
  toCodeLines CallS{proc_id, dagger, args} = ["call" <> dg <> " " <> proc_id <> "(" <> qc <> ");"]
   where
    qc = commaList args
    dg = if dagger then "†" else ""
  toCodeLines (SeqS ps) = concatMap toCodeLines ps

instance ToCodeString ParamTag where
  toCodeString ParamInp = "IN"
  toCodeString ParamOut = "OUT"
  toCodeString ParamAux = "AUX"
  toCodeString ParamUnk = ""

instance (Show a, Show b) => ToCodeString (ProcDef a b) where
  toCodeLines ProcDef{proc_name, proc_params, proc_body} =
    ["uproc " <> proc_name <> "(" <> plist <> ") do"]
      <> indent (toCodeLines proc_body)
      <> ["end"]
   where
    plist = commaList $ map showParam proc_params
    showParam (x, tag, ty) = printf "%s : %s %s" x (toCodeString tag) (toCodeString ty)

instance (Show a) => ToCodeString (OracleDecl a) where
  toCodeString OracleDecl{param_types} =
    printf "uproc Oracle(%s)" plist
   where
    plist = commaList $ map toCodeString param_types

instance (Show a, Show b) => ToCodeString (Program a b) where
  toCodeLines Program{oracle_decl, proc_defs, stmt} = toCodeString oracle_decl : "" : map toCodeString proc_defs <> [toCodeString stmt]

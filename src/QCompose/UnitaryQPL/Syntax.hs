module QCompose.UnitaryQPL.Syntax (
  -- * Syntax

  -- ** Inbuilt functions and unitaries
  ClassicalFun (..),
  Unitary (..),

  -- ** Statements
  MetaParam (..),
  Stmt (..),
  holeS,
  Stmt',

  -- ** Procedures
  ParamTag (..),
  ProcDef (..),
  ProcDef',
  ProcCtx,
  ProcCtx',

  -- ** Program
  Program (..),
  Program',

  -- * Helpers
  HasDagger (..),
) where

import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import Data.Void (Void)
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

data ClassicalFun sizeT
  = ConstF {ty :: P.VarType sizeT, val :: Integer} -- () -> val
  | NotF {ty :: P.VarType sizeT} -- x -> ~x
  | IdF {ty :: P.VarType sizeT} -- x -> x
  | AddF {ty :: P.VarType sizeT} -- x, y -> x + y
  | LEqF {ty :: P.VarType sizeT} -- x, y -> x <= y
  | MultiOrF {cfun_n_args :: sizeT}
  deriving (Eq, Show, Read)

data Unitary sizeT
  = Toffoli
  | CNOT
  | XGate
  | HGate
  | LoadData Ident
  | -- | maps \( |0\rangle \) to \( \frac1{\sqrt{|\Sigma_T|}} \sum_{x \in \Sigma_T} |x\rangle \)
    Unif (P.VarType sizeT)
  | UnifDagger (P.VarType sizeT)
  | -- | reflect about |0>_T
    Refl0 (P.VarType sizeT)
  | RevEmbedU (ClassicalFun sizeT)
  | Controlled (Unitary sizeT)
  deriving (Eq, Show, Read)

class HasDagger a where
  adjoint :: a -> a

instance HasDagger (Unitary sizeT) where
  adjoint (Unif ty) = UnifDagger ty
  adjoint (UnifDagger ty) = Unif ty
  adjoint Toffoli = Toffoli
  adjoint CNOT = CNOT
  adjoint HGate = HGate
  adjoint XGate = XGate
  adjoint (LoadData f) = LoadData f
  adjoint u@(Refl0 _) = u
  adjoint u@(RevEmbedU _) = u
  adjoint (Controlled u) = Controlled (adjoint u)

-- | Compile-time constant parameters
data MetaParam sizeT = MetaName String | MetaSize sizeT
  deriving (Eq, Show, Read)

instance (Show sizeT) => ToCodeString (MetaParam sizeT) where
  toCodeString (MetaName n) = "#" ++ n
  toCodeString (MetaSize n) = show n

data Stmt holeT sizeT
  = SkipS
  | UnitaryS {args :: [Ident], unitary :: Unitary sizeT} -- q... *= U
  | CallS {proc_id :: Ident, dagger :: Bool, args :: [Ident]} -- call F(q...)
  | SeqS [Stmt holeT sizeT] -- W1; W2; ...
  | RepeatS {n_iter :: MetaParam sizeT, loop_body :: Stmt holeT sizeT} -- repeat k do S;
  | HoleS {hole :: holeT, dagger :: Bool} -- temporary place holder
  deriving (Eq, Show, Read)

holeS :: holeT -> Stmt holeT sizeT
holeS hole = HoleS{hole, dagger = False}

-- | Alias for statement without holes
type Stmt' = Stmt Void

instance HasDagger (Stmt holeT sizeT) where
  adjoint SkipS = SkipS
  adjoint s@CallS{dagger} = s{dagger = not dagger}
  adjoint (SeqS ss) = SeqS . reverse $ map adjoint ss
  adjoint s@UnitaryS{unitary} = s{unitary = adjoint unitary}
  adjoint (RepeatS k s) = RepeatS k (adjoint s)
  adjoint (HoleS info dagger) = HoleS info (not dagger)

data ParamTag = ParamInp | ParamOut | ParamAux | ParamUnk deriving (Eq, Show, Read, Enum)

data ProcDef holeT sizeT = ProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_params :: [(Ident, ParamTag, P.VarType sizeT)]
  , mproc_body :: Maybe (Stmt holeT sizeT)
  , is_oracle :: Bool
  }
  deriving (Eq, Show, Read)

-- | A procedure context
type ProcCtx holeT sizeT = Ctx.Context (ProcDef holeT sizeT)

-- | Alias without holes
type ProcDef' = ProcDef Void

-- | Alias without holes
type ProcCtx' sizeT = ProcCtx Void sizeT

-- | A full program
data Program holeT sizeT = Program
  { proc_defs :: ProcCtx holeT sizeT
  , stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

-- | Alias without holes
type Program' = Program Void

-- ================================================================================
-- Printing
-- ================================================================================

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
  toCodeString MultiOrF{cfun_n_args} = printf "(x) => OR_%s(x)" (show cfun_n_args)

instance (Show sizeT) => ToCodeString (Unitary sizeT) where
  toCodeString (RevEmbedU f) = "RevEmbed[" <> toCodeString f <> "]"
  toCodeString (Unif ty) = "Unif[" <> toCodeString ty <> "]"
  toCodeString (UnifDagger ty) = "Unif†[" <> toCodeString ty <> "]"
  toCodeString XGate = "X"
  toCodeString HGate = "H"
  toCodeString (Refl0 ty) = printf "(2|0><0| - I)[%s]" (toCodeString ty)
  toCodeString (LoadData f) = f
  toCodeString u = show u

showDagger :: Bool -> String
showDagger True = "†"
showDagger False = ""

instance (Show holeT, Show sizeT) => ToCodeString (Stmt holeT sizeT) where
  toCodeLines SkipS = ["skip;"]
  toCodeLines UnitaryS{args, unitary} = [qc <> " *= " <> toCodeString unitary <> ";"]
   where
    qc = commaList args
  toCodeLines CallS{proc_id, dagger, args} = [printf "call%s %s(%s);" (showDagger dagger) proc_id qc]
   where
    qc = commaList args
  toCodeLines (SeqS ps) = concatMap toCodeLines ps
  toCodeLines (RepeatS k s) =
    [printf "repeat %s do" (toCodeString k)]
      ++ indent (toCodeLines s)
      ++ ["end"]
  toCodeLines (HoleS info dagger) = [printf "HOLE :: %s%s;" (show info) (showDagger dagger)]

instance ToCodeString ParamTag where
  toCodeString ParamInp = "IN"
  toCodeString ParamOut = "OUT"
  toCodeString ParamAux = "AUX"
  toCodeString ParamUnk = ""

showParamWithTag :: (Show sizeT) => (Ident, ParamTag, P.VarType sizeT) -> String
showParamWithTag (x, tag, ty) = printf "%s : %s%s" x tag_s (toCodeString ty)
 where
  tag_s = case toCodeString tag of
    "" -> ""
    s -> s ++ " "

instance (Show holeT, Show sizeT) => ToCodeString (ProcDef holeT sizeT) where
  toCodeLines ProcDef{proc_name, proc_meta_params, proc_params, mproc_body, is_oracle} =
    ["@Oracle" | is_oracle]
      ++ case mproc_body of
        Nothing -> [printf "%s;" header]
        Just proc_body ->
          [printf "%s do" header]
            <> indent (toCodeLines proc_body)
            <> ["end"]
   where
    mplist, plist, header :: String
    mplist = commaList $ map ("#" ++) proc_meta_params
    plist = commaList $ map showParamWithTag proc_params
    header = printf "uproc %s[%s](%s)" proc_name mplist plist

instance (Show holeT, Show sizeT) => ToCodeString (Program holeT sizeT) where
  toCodeLines Program{proc_defs, stmt} = map toCodeString (Ctx.elems proc_defs) <> [toCodeString stmt]

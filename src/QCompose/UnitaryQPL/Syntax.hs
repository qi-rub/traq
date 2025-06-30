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

  -- ** Syntax Sugar
  mkForInRangeS,
  desugarS,

  -- * Helpers
  HasDagger (..),
) where

import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import Data.Void (Void)
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

-- | Compile-time constant parameters
data MetaParam sizeT = MetaName String | MetaSize sizeT | MetaValue Value
  deriving (Eq, Show, Read)

instance (Show sizeT) => ToCodeString (MetaParam sizeT) where
  toCodeString (MetaName n) = "#" ++ n
  toCodeString (MetaSize n) = show n
  toCodeString (MetaValue n) = show n

data ClassicalFun sizeT
  = ConstF {ty :: P.VarType sizeT, val :: MetaParam sizeT} -- () -> val
  | NotF {ty :: P.VarType sizeT} -- x -> ~x
  | IdF {ty :: P.VarType sizeT} -- x -> x
  | AddF {ty :: P.VarType sizeT} -- x, y -> x + y
  | LEqF {ty :: P.VarType sizeT} -- x, y -> x <= y
  | LEqConstF {val :: MetaParam sizeT, ty :: P.VarType sizeT} -- x -> x <= val
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

data Stmt holeT sizeT
  = SkipS
  | UnitaryS {args :: [Ident], unitary :: Unitary sizeT} -- q... *= U
  | CallS {proc_id :: Ident, dagger :: Bool, args :: [Ident]} -- call F(q...)
  | SeqS [Stmt holeT sizeT] -- W1; W2; ...
  | -- placeholders
    HoleS {hole :: holeT, dagger :: Bool} -- temporary place holder
  | CommentS String
  | -- syntax sugar
    RepeatS {n_iter :: MetaParam sizeT, loop_body :: Stmt holeT sizeT} -- repeat k do S;
  | ForInRangeS
      { iter_meta_var :: Ident
      , iter_lim :: MetaParam sizeT
      , loop_body :: Stmt holeT sizeT
      , dagger :: Bool
      }
  deriving (Eq, Show, Read)

mkForInRangeS :: Ident -> MetaParam sizeT -> Stmt holeT sizeT -> Stmt holeT sizeT
mkForInRangeS iter_meta_var iter_lim loop_body = ForInRangeS{iter_meta_var, iter_lim, loop_body, dagger = False}

holeS :: holeT -> Stmt holeT sizeT
holeS hole = HoleS{hole, dagger = False}

-- | Alias for statement without holes
type Stmt' = Stmt Void

instance HasDagger (Stmt holeT sizeT) where
  adjoint s@(CommentS _) = s
  adjoint SkipS = SkipS
  adjoint s@CallS{dagger} = s{dagger = not dagger}
  adjoint (SeqS ss) = SeqS . reverse $ map adjoint ss
  adjoint s@UnitaryS{unitary} = s{unitary = adjoint unitary}
  adjoint (RepeatS k s) = RepeatS k (adjoint s)
  adjoint (HoleS info dagger) = HoleS info (not dagger)
  adjoint s@ForInRangeS{dagger} = s{dagger = not dagger}

data ParamTag = ParamCtrl | ParamInp | ParamOut | ParamAux | ParamUnk deriving (Eq, Show, Read, Enum)

data ProcDef holeT sizeT costT = ProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_params :: [(Ident, ParamTag, P.VarType sizeT)]
  , proc_body_or_tick :: Either costT (Stmt holeT sizeT) -- Left tick | Right body
  }
  deriving (Eq, Show, Read)

-- | A procedure context
type ProcCtx holeT sizeT costT = Ctx.Context (ProcDef holeT sizeT costT)

-- | Alias without holes
type ProcDef' sizeT costT = ProcDef Void sizeT costT

-- | Alias without holes
type ProcCtx' sizeT costT = ProcCtx Void sizeT costT

-- | A full program
data Program holeT sizeT costT = Program
  { proc_defs :: ProcCtx holeT sizeT costT
  , stmt :: Stmt holeT sizeT
  }
  deriving (Eq, Show, Read)

-- | Alias without holes
type Program' = Program Void

-- ================================================================================
-- Syntax Sugar
-- ================================================================================

desugarS :: Stmt holeT sizeT -> Maybe (Stmt holeT sizeT)
desugarS _ = Nothing

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
  toCodeString LEqConstF{val, ty} = printf "%s => x≤%s" (showTypedIdent ("x", ty)) (toCodeString val)
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
  toCodeLines (CommentS c) = ["// " ++ c]
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
  -- syntax sugar
  toCodeLines ForInRangeS{iter_meta_var, iter_lim, dagger, loop_body} =
    printf "for #%s in %s do" iter_meta_var range_str
      : indent (toCodeLines loop_body)
      ++ ["end"]
   where
    range_str :: String
    range_str
      | dagger = printf "%s - 1 .. 0" (toCodeString iter_lim)
      | otherwise = printf "0 .. < %s" (toCodeString iter_lim)

-- { iter_meta_var :: Ident
-- , iter_lim :: MetaParam sizeT
-- , loop_body :: Stmt holeT sizeT
-- , dagger :: Bool
-- }

instance ToCodeString ParamTag where
  toCodeString ParamCtrl = "CTRL"
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

instance (Show holeT, Show sizeT, Show costT) => ToCodeString (ProcDef holeT sizeT costT) where
  toCodeLines ProcDef{proc_name, proc_meta_params, proc_params, proc_body_or_tick} =
    case proc_body_or_tick of
      Left tick -> [printf "%s :: tick(%s)" header (show tick)]
      Right proc_body ->
        [printf "%s do" header]
          <> indent (toCodeLines proc_body)
          <> ["end"]
   where
    mplist, plist, header :: String
    mplist = commaList $ map ("#" ++) proc_meta_params
    plist = commaList $ map showParamWithTag proc_params
    header = printf "uproc %s[%s](%s)" proc_name mplist plist

instance (Show holeT, Show sizeT, Show costT) => ToCodeString (Program holeT sizeT costT) where
  toCodeLines Program{proc_defs, stmt} = map toCodeString (Ctx.elems proc_defs) <> [toCodeString stmt]

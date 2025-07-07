module Traq.UnitaryQPL.Syntax (
  -- * Syntax

  -- ** Inbuilt functions and unitaries
  Unitary (..),

  -- ** Statements
  UStmt (..),
  holeS,
  UStmt',

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

import Data.Void (Void)
import Text.Printf (printf)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import Traq.Utils.Printing

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
  | RevEmbedU [Ident] (P.BasicExpr sizeT)
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
  adjoint u@(RevEmbedU _ _) = u
  adjoint (Controlled u) = Controlled (adjoint u)

data UStmt holeT sizeT
  = USkipS
  | UnitaryS {args :: [Ident], unitary :: Unitary sizeT} -- q... *= U
  | UCallS {proc_id :: Ident, dagger :: Bool, args :: [Ident]} -- call F(q...)
  | USeqS [UStmt holeT sizeT] -- W1; W2; ...
  | -- placeholders
    UHoleS {hole :: holeT, dagger :: Bool} -- temporary place holder
  | UCommentS String
  | -- syntax sugar
    URepeatS {n_iter :: P.MetaParam sizeT, loop_body :: UStmt holeT sizeT} -- repeat k do S;
  | UForInRangeS
      { iter_meta_var :: Ident
      , iter_lim :: P.MetaParam sizeT
      , loop_body :: UStmt holeT sizeT
      , dagger :: Bool
      }
  | UWithComputedS {with_stmt, body_stmt :: UStmt holeT sizeT}
  deriving (Eq, Show, Read)

mkForInRangeS :: Ident -> P.MetaParam sizeT -> UStmt holeT sizeT -> UStmt holeT sizeT
mkForInRangeS iter_meta_var iter_lim loop_body = UForInRangeS{iter_meta_var, iter_lim, loop_body, dagger = False}

holeS :: holeT -> UStmt holeT sizeT
holeS hole = UHoleS{hole, dagger = False}

-- | Alias for statement without holes
type UStmt' = UStmt Void

instance HasDagger (UStmt holeT sizeT) where
  adjoint s@(UCommentS _) = s
  adjoint USkipS = USkipS
  adjoint s@UCallS{dagger} = s{dagger = not dagger}
  adjoint (USeqS ss) = USeqS . reverse $ map adjoint ss
  adjoint s@UnitaryS{unitary} = s{unitary = adjoint unitary}
  adjoint (URepeatS k s) = URepeatS k (adjoint s)
  adjoint (UHoleS info dagger) = UHoleS info (not dagger)
  adjoint s@UForInRangeS{dagger} = s{dagger = not dagger}
  adjoint s@UWithComputedS{body_stmt} = s{body_stmt = adjoint body_stmt}

data ParamTag = ParamCtrl | ParamInp | ParamOut | ParamAux | ParamUnk deriving (Eq, Show, Read, Enum)

data ProcDef holeT sizeT costT = UProcDef
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_params :: [(Ident, ParamTag, P.VarType sizeT)]
  , proc_body_or_tick :: Either costT (UStmt holeT sizeT) -- Left tick | Right body
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
  , stmt :: UStmt holeT sizeT
  }
  deriving (Eq, Show, Read)

-- | Alias without holes
type Program' = Program Void

-- ================================================================================
-- Syntax Sugar
-- ================================================================================

desugarS :: UStmt holeT sizeT -> Maybe (UStmt holeT sizeT)
desugarS UWithComputedS{with_stmt, body_stmt} = Just $ USeqS [with_stmt, body_stmt, adjoint with_stmt]
desugarS _ = Nothing

-- ================================================================================
-- Printing
-- ================================================================================

instance (Show sizeT) => ToCodeString (Unitary sizeT) where
  toCodeString (RevEmbedU xs e) = printf "Embed[(%s) => %s]" (commaList xs) (toCodeString e)
  toCodeString (Unif ty) = "Unif[" <> toCodeString ty <> "]"
  toCodeString (UnifDagger ty) = "Adj-Unif[" <> toCodeString ty <> "]"
  toCodeString XGate = "X"
  toCodeString HGate = "H"
  toCodeString (Refl0 ty) = printf "Refl0[%s]" (toCodeString ty)
  toCodeString (LoadData f) = f
  toCodeString (Controlled u) = "Ctrl-" <> toCodeString u
  toCodeString u = show u

showDagger :: Bool -> String
showDagger True = "-adj"
showDagger False = ""

instance (Show holeT, Show sizeT) => ToCodeString (UStmt holeT sizeT) where
  toCodeLines USkipS = ["skip;"]
  toCodeLines (UCommentS c) = ["// " ++ c]
  toCodeLines UnitaryS{args, unitary} = [qc <> " *= " <> toCodeString unitary <> ";"]
   where
    qc = commaList args
  toCodeLines UCallS{proc_id, dagger, args} = [printf "call%s %s(%s);" (showDagger dagger) proc_id qc]
   where
    qc = commaList args
  toCodeLines (USeqS ps) = concatMap toCodeLines ps
  toCodeLines (URepeatS k s) =
    [printf "repeat %s do" (toCodeString k)]
      ++ indent (toCodeLines s)
      ++ ["end"]
  toCodeLines (UHoleS info dagger) = [printf "HOLE :: %s%s;" (show info) (showDagger dagger)]
  -- syntax sugar
  toCodeLines UForInRangeS{iter_meta_var, iter_lim, dagger, loop_body} =
    printf "for #%s in %s do" iter_meta_var range_str
      : indent (toCodeLines loop_body)
      ++ ["end"]
   where
    range_str :: String
    range_str
      | dagger = printf "%s - 1 .. 0" (toCodeString iter_lim)
      | otherwise = printf "0 .. < %s" (toCodeString iter_lim)
  toCodeLines UWithComputedS{with_stmt, body_stmt} =
    "with {" : indent (toCodeLines with_stmt) ++ ["} do {"] ++ indent (toCodeLines body_stmt) ++ ["}"]

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
  toCodeLines UProcDef{proc_name, proc_meta_params, proc_params, proc_body_or_tick} =
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

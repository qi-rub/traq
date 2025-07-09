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
import qualified Traq.Utils.Printing as PP

data Unitary sizeT
  = Toffoli
  | CNOT
  | XGate
  | HGate
  | LoadData Ident
  | -- | maps \( |0\rangle \) to \( \frac1{\sqrt{|\Sigma_T|}} \sum_{x \in \Sigma_T} |x\rangle \)
    Unif
  | -- | reflect about |0>_T
    Refl0
  | RevEmbedU [Ident] (P.BasicExpr sizeT)
  | Controlled (Unitary sizeT)
  | Adjoint (Unitary sizeT)
  deriving (Eq, Show, Read)

class HasDagger a where
  adjoint :: a -> a

instance HasDagger (Unitary sizeT) where
  adjoint Toffoli = Toffoli
  adjoint CNOT = CNOT
  adjoint HGate = HGate
  adjoint XGate = XGate
  adjoint (LoadData f) = LoadData f
  adjoint Refl0 = Refl0
  adjoint u@(RevEmbedU _ _) = u
  adjoint (Controlled u) = Controlled (adjoint u)
  adjoint (Adjoint u) = u
  adjoint u = Adjoint u

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
  { info_comment :: String
  , proc_name :: Ident
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

instance (Show sizeT) => PP.ToCodeString (Unitary sizeT) where
  toCodeString (RevEmbedU xs e) = printf "Embed[(%s) => %s]" (PP.commaList xs) (PP.toCodeString e)
  toCodeString Unif = "Unif"
  toCodeString XGate = "X"
  toCodeString HGate = "H"
  toCodeString Refl0 = printf "Refl0"
  toCodeString (LoadData f) = f
  toCodeString (Controlled u) = "Ctrl-" <> PP.toCodeString u
  toCodeString (Adjoint u) = "Adj-" <> PP.toCodeString u
  toCodeString u = show u

showDagger :: Bool -> String
showDagger True = "-adj"
showDagger False = ""

instance (Show holeT, Show sizeT) => PP.ToCodeString (UStmt holeT sizeT) where
  build USkipS = PP.putLine "skip;"
  build (UCommentS c) = PP.putComment c
  build UnitaryS{args, unitary} = PP.concatenated $ do
    PP.put $ PP.commaList args
    PP.put " *= "
    PP.build unitary
    PP.put ";"
  build UCallS{proc_id, dagger, args} = PP.concatenated $ do
    PP.put "call"
    PP.put $ showDagger dagger
    PP.put " "
    PP.put proc_id
    PP.put $ PP.commaList args
    PP.put ";"
  build (USeqS ps) = mapM_ PP.build ps
  -- syntax sugar
  build (UHoleS info dagger) = PP.putLine $ printf "HOLE :: %s%s;" (show info) (showDagger dagger)
  build (URepeatS k s) = do
    let header = printf "repeat %s" (PP.toCodeString k)
    PP.bracedBlockWith header $ PP.build s
  build UWithComputedS{with_stmt, body_stmt} = do
    PP.bracedBlockWith "with" $ PP.build with_stmt
    PP.bracedBlockWith "do" $ PP.build body_stmt
  build UForInRangeS{iter_meta_var, iter_lim, dagger, loop_body} = do
    let header = printf "for #%s in %s" iter_meta_var range_str
    PP.bracedBlockWith header $ PP.build loop_body
   where
    range_str :: String
    range_str
      | dagger = printf "%s - 1 .. 0" (PP.toCodeString iter_lim)
      | otherwise = printf "0 .. < %s" (PP.toCodeString iter_lim)

instance PP.ToCodeString ParamTag where
  toCodeString ParamCtrl = "CTRL"
  toCodeString ParamInp = "IN"
  toCodeString ParamOut = "OUT"
  toCodeString ParamAux = "AUX"
  toCodeString ParamUnk = ""

showParamWithTag :: (Show sizeT) => (Ident, ParamTag, P.VarType sizeT) -> String
showParamWithTag (x, tag, ty) = printf "%s : %s%s" x tag_s (PP.toCodeString ty)
 where
  tag_s = case PP.toCodeString tag of
    "" -> ""
    s -> s ++ " "

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (ProcDef holeT sizeT costT) where
  build UProcDef{info_comment, proc_name, proc_meta_params, proc_params, proc_body_or_tick} = do
    PP.putComment info_comment
    case proc_body_or_tick of
      Left tick -> PP.putLine $ printf "%s :: tick(%s);" header (show tick)
      Right proc_body -> PP.bracedBlockWith header $ PP.build proc_body
   where
    mplist, plist, header :: String
    mplist = PP.wrapNonEmpty "[" "]" $ PP.commaList $ map ("#" ++) proc_meta_params
    plist = PP.commaList $ map showParamWithTag proc_params
    header = printf "uproc %s%s(%s)" proc_name mplist plist

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (Program holeT sizeT costT) where
  toCodeLines Program{proc_defs, stmt} = map PP.toCodeString (Ctx.elems proc_defs) <> [PP.toCodeString stmt]

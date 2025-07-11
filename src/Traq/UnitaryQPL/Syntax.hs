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
  HasAdjoint (..),
) where

import Control.Monad (forM, (>=>))
import Data.Void (Void)
import Lens.Micro.GHC
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

class HasAdjoint a where
  adjoint :: a -> a

instance HasAdjoint (Unitary sizeT) where
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
    URepeatS {n_iter :: MetaParam sizeT, loop_body :: UStmt holeT sizeT} -- repeat k do S;
  | UForInRangeS
      { iter_meta_var :: Ident
      , iter_lim :: MetaParam sizeT
      , loop_body :: UStmt holeT sizeT
      , dagger :: Bool
      }
  | UWithComputedS {with_stmt, body_stmt :: UStmt holeT sizeT}
  deriving (Eq, Show, Read)

mkForInRangeS :: Ident -> MetaParam sizeT -> UStmt holeT sizeT -> UStmt holeT sizeT
mkForInRangeS iter_meta_var iter_lim loop_body = UForInRangeS{iter_meta_var, iter_lim, loop_body, dagger = False}

holeS :: holeT -> UStmt holeT sizeT
holeS hole = UHoleS{hole, dagger = False}

-- | Alias for statement without holes
type UStmt' = UStmt Void

instance HasAdjoint (UStmt holeT sizeT) where
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
  build (RevEmbedU xs e) = do
    e_s <- PP.fromBuild e
    PP.putWord $ printf "Embed[(%s) => %s]" (PP.commaList xs) e_s
  build Unif = PP.putWord "Unif"
  build XGate = PP.putWord "X"
  build HGate = PP.putWord "H"
  build Refl0 = PP.putWord $ printf "Refl0"
  build (LoadData f) = PP.putWord f
  build (Controlled u) = PP.putWord . ("Ctrl-" <>) =<< PP.fromBuild u
  build (Adjoint u) = PP.putWord . ("Adj-" <>) =<< PP.fromBuild u
  build u = PP.putWord $ show u

showDagger :: Bool -> String
showDagger True = "-adj"
showDagger False = ""

instance (Show holeT, Show sizeT) => PP.ToCodeString (UStmt holeT sizeT) where
  build USkipS = PP.putLine "skip;"
  build (UCommentS c) = PP.putComment c
  build UnitaryS{args, unitary} = PP.concatenated $ do
    PP.putWord $ PP.commaList args
    PP.putWord " *= "
    PP.build unitary
    PP.putWord ";"
  build UCallS{proc_id, dagger, args} = PP.concatenated $ do
    PP.putWord "call"
    PP.putWord $ showDagger dagger
    PP.putWord " "
    PP.putWord proc_id
    PP.putWord $ PP.commaList args
    PP.putWord ";"
  build (USeqS ps) = mapM_ PP.build ps
  -- syntax sugar
  build (UHoleS info dagger) = PP.putLine $ printf "HOLE :: %s%s;" (show info) (showDagger dagger)
  build (URepeatS k s) = do
    header <- printf "repeat (%s)" <$> PP.fromBuild k
    PP.bracedBlockWith header $ PP.build s
  build UWithComputedS{with_stmt, body_stmt} = do
    PP.bracedBlockWith "with" $ PP.build with_stmt
    PP.bracedBlockWith "do" $ PP.build body_stmt
  build UForInRangeS{iter_meta_var, iter_lim, dagger, loop_body} = do
    iter_lim_s <- PP.fromBuild iter_lim
    let ss = printf range_str iter_lim_s :: String
    let header = printf "for (#%s in %s)" iter_meta_var ss
    PP.bracedBlockWith header $ PP.build loop_body
   where
    range_str :: String
    range_str | dagger = "%s - 1 .. 0" | otherwise = "0 .. < %s"

instance PP.ToCodeString ParamTag where
  build ParamCtrl = PP.putWord "CTRL"
  build ParamInp = PP.putWord "IN"
  build ParamOut = PP.putWord "OUT"
  build ParamAux = PP.putWord "AUX"
  build ParamUnk = PP.putWord ""

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (ProcDef holeT sizeT costT) where
  build UProcDef{info_comment, proc_name, proc_meta_params, proc_params, proc_body_or_tick} = do
    PP.putComment info_comment

    proc_params_s <- forM proc_params $ \(x, tag, ty) -> do
      tag_s <- PP.fromBuild tag <&> \case "" -> ""; s -> s ++ " "
      printf "%s : %s%s" x tag_s <$> PP.fromBuild ty
    let mplist = PP.wrapNonEmpty "[" "]" $ PP.commaList $ map ("#" ++) proc_meta_params
    let plist = PP.commaList proc_params_s
    let header = printf "uproc %s%s(%s)" proc_name mplist plist

    case proc_body_or_tick of
      Left tick -> PP.putLine $ printf "%s :: tick(%s);" header (show tick)
      Right proc_body -> do
        PP.bracedBlockWith header $ PP.build proc_body

instance (Show holeT, Show sizeT, Show costT) => PP.ToCodeString (Program holeT sizeT costT) where
  build Program{proc_defs, stmt} = do
    mapM_ (PP.build >=> const PP.endl) (Ctx.elems proc_defs)
    PP.build stmt

module Traq.UnitaryQPL.Syntax (
  -- * Syntax

  -- ** Inbuilt functions and unitaries
  Unitary (..),

  -- ** Statements
  UStmt (..),
  UStmt',

  -- ** Syntax Sugar
  mkForInRangeS,

  -- * Helpers
  HasAdjoint (..),
) where

import Data.Void (Void)
import Text.Printf (printf)

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Transformers
-- ================================================================================
class HasAdjoint a where
  adjoint :: a -> a

-- ================================================================================
-- Syntax
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Unitary Operators
-- --------------------------------------------------------------------------------

-- | Unitary operators in CQPL
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

-- --------------------------------------------------------------------------------
-- Unitary Statements
-- --------------------------------------------------------------------------------

-- | Unitary Statement
data UStmt holeT sizeT
  = USkipS
  | UnitaryS {qargs :: [Ident], unitary :: Unitary sizeT} -- q... *= U
  | UCallS {uproc_id :: Ident, dagger :: Bool, qargs :: [Ident]} -- call F(q...)
  | USeqS [UStmt holeT sizeT] -- W1; W2; ...
  | -- placeholders
    UHoleS {uhole :: holeT, dagger :: Bool} -- temporary place holder
  | UCommentS String
  | -- syntax sugar
    URepeatS {n_iter :: P.MetaParam sizeT, uloop_body :: UStmt holeT sizeT} -- repeat k do S;
  | UForInRangeS
      { iter_meta_var :: Ident
      , iter_lim :: P.MetaParam sizeT
      , uloop_body :: UStmt holeT sizeT
      , dagger :: Bool
      }
  | UWithComputedS {with_ustmt, body_ustmt :: UStmt holeT sizeT}
  deriving (Eq, Show, Read)

mkForInRangeS :: Ident -> P.MetaParam sizeT -> UStmt holeT sizeT -> UStmt holeT sizeT
mkForInRangeS iter_meta_var iter_lim uloop_body = UForInRangeS{iter_meta_var, iter_lim, uloop_body, dagger = False}

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
  adjoint s@UWithComputedS{body_ustmt = b} = s{body_ustmt = adjoint b}

showDagger :: Bool -> String
showDagger True = "-adj"
showDagger False = ""

instance (Show holeT, Show sizeT) => PP.ToCodeString (UStmt holeT sizeT) where
  build USkipS = PP.putLine "skip;"
  build (UCommentS c) = PP.putComment c
  build UnitaryS{qargs, unitary} = PP.concatenated $ do
    PP.putWord $ PP.commaList qargs
    PP.putWord " *= "
    PP.build unitary
    PP.putWord ";"
  build UCallS{uproc_id, dagger, qargs} = PP.concatenated $ do
    PP.putWord "call"
    PP.putWord $ showDagger dagger
    PP.putWord " "
    PP.putWord uproc_id
    PP.putWord $ PP.commaList qargs
    PP.putWord ";"
  build (USeqS ps) = mapM_ PP.build ps
  -- syntax sugar
  build (UHoleS info dagger) = PP.putLine $ printf "HOLE :: %s%s;" (show info) (showDagger dagger)
  build (URepeatS k s) = do
    header <- printf "repeat (%s)" <$> PP.fromBuild k
    PP.bracedBlockWith header $ PP.build s
  build UWithComputedS{with_ustmt, body_ustmt} = do
    PP.bracedBlockWith "with" $ PP.build with_ustmt
    PP.bracedBlockWith "do" $ PP.build body_ustmt
  build UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = do
    iter_lim_s <- PP.fromBuild iter_lim
    let ss = printf range_str iter_lim_s :: String
    let header = printf "for (#%s in %s)" iter_meta_var ss
    PP.bracedBlockWith header $ PP.build uloop_body
   where
    range_str :: String
    range_str | dagger = "%s - 1 .. 0" | otherwise = "0 .. < %s"

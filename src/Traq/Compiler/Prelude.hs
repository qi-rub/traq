{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Traq.Compiler.Prelude (
  -- * Utilities for generating identifiers
  UniqNamesCtx,
  HasUniqNamesCtx (..),
  newIdent,
  mkQProcName,
  mkUProcName,

  -- * Compilation Monad
  CompilerT,

  -- ** State
  LoweringCtx,
  ProcSignature (..),
  _procSignatures,

  -- ** Output
  LoweringOutput,
  _loweredProcs,
  addProc,

  -- ** Env
  LoweringEnv,
) where

import Control.Monad.Except (MonadError)
import Control.Monad.Extra (loopM)
import Control.Monad.RWS (RWST)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import Traq.Data.Default

import qualified Traq.CQPL as CQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P

{- | A set of already used names.
 This is used to generate new unique identifiers.
-}
type UniqNamesCtx = Set.Set Ident

class HasUniqNamesCtx s where
  _uniqNamesCtx :: Lens' s UniqNamesCtx

instance HasUniqNamesCtx UniqNamesCtx where _uniqNamesCtx = id

-- | Generate a new identifier with the given prefix.
newIdent ::
  forall s m.
  ( MonadError String m
  , MonadState s m
  , HasUniqNamesCtx s
  ) =>
  Ident ->
  m Ident
newIdent prefix = do
  ident <- loopM checked 0
  _uniqNamesCtx . at ident ?= ()
  return ident
 where
  checked :: Int -> m (Either Int Ident)
  checked i = do
    let name = prefix <> (if i > 0 then "_" <> show i else "")
    already_exists <- use (_uniqNamesCtx . at name)
    return $ case already_exists of
      Nothing -> Right name
      Just () -> Left $ i + 1

-- | get the name of the compiled (cq) proc given the source fun name
mkQProcName :: Ident -> Ident
mkQProcName s = s

-- | get the name of the compiled uproc given the source fun name
mkUProcName :: Ident -> Ident
mkUProcName s = s ++ "_U"

-- ================================================================================
-- Compiler State
-- ================================================================================

-- | Signature of a compiled uproc/proc: inputs, outputs, ancilla (for unitary)
data ProcSignature size = ProcSignature {in_tys, out_tys, aux_tys :: [P.VarType size]}

-- | A global lowering context.
data LoweringCtx sizeT
  = LoweringCtx
      -- | The set of already used identifiers (to generate unique ones)
      (Set.Set Ident)
      -- | The typing context: mapping all variables in context to their types.
      (P.TypingCtx sizeT)
      -- | Signature of each uproc
      (Map.Map Ident (ProcSignature sizeT))
  deriving (Generic, HasDefault)

type instance SizeType (LoweringCtx sizeT) = sizeT

instance HasUniqNamesCtx (LoweringCtx sizeT) where
  _uniqNamesCtx focus (LoweringCtx a b c) = focus a <&> \a' -> LoweringCtx a' b c

instance P.HasTypingCtx (LoweringCtx sizeT) where
  _typingCtx focus (LoweringCtx a b c) = focus b <&> \b' -> LoweringCtx a b' c

_procSignatures :: Lens' (LoweringCtx size) (Map.Map Ident (ProcSignature size))
_procSignatures focus (LoweringCtx a b c) = focus c <&> \c' -> LoweringCtx a b c'

-- ================================================================================
-- Compiler Output
-- ================================================================================

-- | The outputs of lowering
newtype LoweringOutput size
  = LoweringOutput
      [CQPL.ProcDef size]
  deriving newtype (Semigroup, Monoid)

_loweredProcs :: Lens' (LoweringOutput size) [CQPL.ProcDef size]
_loweredProcs focus (LoweringOutput a) = focus a <&> LoweringOutput

addProc :: (MonadWriter (LoweringOutput size) m) => CQPL.ProcDef size -> m ()
addProc = writeElemAt _loweredProcs

-- ================================================================================
-- Compiler Environment
-- ================================================================================

-- | Read-only compiler env
type LoweringEnv ext = P.FunCtx ext

-- ================================================================================
-- Compiler Monad
-- ================================================================================

{- | Monad to compile source programs to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT ext =
  RWST
    (LoweringEnv ext)
    (LoweringOutput (SizeType ext))
    (LoweringCtx (SizeType ext))
    (Either String)

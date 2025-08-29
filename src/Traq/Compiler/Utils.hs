{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Traq.Compiler.Utils (
  -- * Utilities for generating identifiers
  UniqNamesCtx,
  HasUniqNamesCtx (..),
  newIdent,

  -- * Environments for compilation

  -- ** State
  LoweringCtx,

  -- ** Output
  LoweringOutput,
  _loweredProcs,
  addProc,
) where

import Control.Monad (MonadPlus, msum)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
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
  , MonadPlus m
  ) =>
  Ident ->
  m Ident
newIdent prefix = do
  ident <-
    msum . map checked $
      prefix : map ((prefix <>) . ("_" <>) . show) [1 :: Int ..]
  _uniqNamesCtx . at ident ?= ()
  return ident
 where
  checked :: Ident -> m Ident
  checked name = do
    already_exists <- use (_uniqNamesCtx . at name)
    case already_exists of
      Nothing -> return name
      Just () -> throwError "next ident please!"

-- ================================================================================
-- Compiler State
-- ================================================================================

{- | A global lowering context, consisting of
- The set of already used identifiers (to generate unique ones)
- The typing context: mapping all variables in context to their types.
-}
data LoweringCtx sizeT = LoweringCtx (Set.Set Ident) (P.TypingCtx sizeT)
  deriving (Generic, HasDefault)

type instance SizeType (LoweringCtx sizeT) = sizeT

instance HasUniqNamesCtx (LoweringCtx sizeT) where
  _uniqNamesCtx focus (LoweringCtx a b) = focus a <&> \a' -> LoweringCtx a' b

instance P.HasTypingCtx (LoweringCtx sizeT) where
  _typingCtx focus (LoweringCtx a b) = focus b <&> \b' -> LoweringCtx a b'

-- ================================================================================
-- Compiler Output
-- ================================================================================

-- | The outputs of lowering
type LoweringOutput sizeT costT = [CQPL.ProcDef sizeT costT]

_loweredProcs :: Lens' (LoweringOutput sizeT costT) [CQPL.ProcDef sizeT costT]
_loweredProcs = id

addProc ::
  (MonadWriter (LoweringOutput sizeT costT) m) =>
  CQPL.ProcDef sizeT costT ->
  m ()
addProc = writeElemAt _loweredProcs

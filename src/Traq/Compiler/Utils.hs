{-# LANGUAGE FlexibleInstances #-}

module Traq.Compiler.Utils (
  -- * Utilities for generating identifiers
  UniqNamesCtx,
  HasUniqNamesCtx (..),
  newIdent,

  -- * Environments for compilation
  LoweringCtx,
) where

import Control.Monad (MonadPlus, msum)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState)
import qualified Data.Set as Set
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Data.Default

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

{- | A global lowering context, consisting of
- The set of already used identifiers (to generate unique ones)
- The typing context: mapping all variables in context to their types.
-}
data LoweringCtx sizeT = LoweringCtx (Set.Set Ident) (P.TypingCtx sizeT)

instance HasDefault (LoweringCtx sizeT) where default_ = LoweringCtx default_ default_

type instance SizeType (LoweringCtx sizeT) = sizeT

instance HasUniqNamesCtx (LoweringCtx sizeT) where
  _uniqNamesCtx focus (LoweringCtx a b) = focus a <&> \a' -> LoweringCtx a' b

instance P.HasTypingCtx (LoweringCtx sizeT) where
  _typingCtx focus (LoweringCtx a b) = focus b <&> \b' -> LoweringCtx a b'

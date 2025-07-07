{-# LANGUAGE FlexibleInstances #-}

module Traq.Compiler.Utils (
  UniqNamesCtx,
  HasUniqNamesCtx (..),

  -- * Environments for compilation
  LoweringCtx,
) where

import qualified Data.Set as Set
import Lens.Micro.GHC

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

{- | A global lowering context, consisting of
- The set of already used identifiers (to generate unique ones)
- The typing context: mapping all variables in context to their types.
-}
data LoweringCtx sizeT = LoweringCtx (Set.Set Ident) (P.TypingCtx sizeT)

instance HasDefault (LoweringCtx sizeT) where default_ = LoweringCtx default_ default_

type instance P.SizeType (LoweringCtx sizeT) = sizeT

instance HasUniqNamesCtx (LoweringCtx sizeT) where
  _uniqNamesCtx focus (LoweringCtx a b) = focus a <&> \a' -> LoweringCtx a' b

instance P.HasTypingCtx (LoweringCtx sizeT) where
  _typingCtx focus (LoweringCtx a b) = focus b <&> \b' -> LoweringCtx a b'

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Traq.Primitives.Class.TypeCheck (
  TypeCheckPrim (..),
) where

import Control.Monad.Except (liftEither)
import GHC.Generics

import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives.Class.Prelude

-- --------------------------------------------------------------------------------
-- Typing
-- --------------------------------------------------------------------------------

{- | Type check a primitive given the types of its function arguments, and infer the return types.
The typechecker internally checks that the bound arguments are correct,
and only gives the user the final type of the partial function.
-}
class
  ( size ~ SizeType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  TypeCheckPrim prim size
    | prim -> size
  where
  inferRetTypesPrim ::
    forall ext' shape m.
    ( m ~ CPL.TypeChecker ext'
    , size ~ SizeType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape (CPL.FnType size) ->
    m [CPL.VarType size]
  default inferRetTypesPrim ::
    forall ext' shape m.
    ( Generic prim
    , GTypeCheckPrim (Rep prim) size
    , m ~ CPL.TypeChecker ext'
    , size ~ SizeType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape (CPL.FnType size) ->
    m [CPL.VarType size]
  inferRetTypesPrim prim = ginferRetTypesPrim (from prim)

class GTypeCheckPrim f size where
  ginferRetTypesPrim ::
    forall ext' shape m p.
    ( m ~ CPL.TypeChecker ext'
    , size ~ SizeType ext'
    , ValidPrimShape shape
    ) =>
    f p ->
    shape (CPL.FnType size) ->
    m [CPL.VarType size]

instance (GTypeCheckPrim a size, GTypeCheckPrim b size) => GTypeCheckPrim (a :+: b) size where
  ginferRetTypesPrim (L1 x) = ginferRetTypesPrim x
  ginferRetTypesPrim (R1 x) = ginferRetTypesPrim x

instance (GTypeCheckPrim f size) => GTypeCheckPrim (M1 i c f) size where
  ginferRetTypesPrim (M1 x) = ginferRetTypesPrim x

instance (TypeCheckPrim a size) => GTypeCheckPrim (K1 i a) size where
  ginferRetTypesPrim (K1 x) fs = do
    fs' <- liftEither $ reshape fs
    inferRetTypesPrim x fs'

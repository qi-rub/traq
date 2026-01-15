{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.TypeCheck (
  TypeCheckPrim (..),
) where

import Traq.Prelude
import Traq.Primitives.Class.Prelude
import qualified Traq.ProtoLang as P

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
    ( m ~ P.TypeChecker ext'
    , size ~ SizeType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape (P.FnType size) ->
    m [P.VarType size]

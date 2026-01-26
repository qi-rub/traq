{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.UnitaryCompile (
  UnitaryCompilePrim (..),
  UnitaryCompilePrimBuilder (..),
) where

import Control.Monad.RWS (RWST (..))
import Control.Monad.Trans (lift)
import GHC.Generics

import Lens.Micro.Mtl

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import Traq.Compiler
import Traq.Prelude
import Traq.Primitives.Class.Prelude
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Unitary Compilation
-- --------------------------------------------------------------------------------

type UCallBuilder size = [Ident] -> CQPL.UStmt size

-- type UProcBuilder size = [(Ident, P.VarType size)] -> CQPL.UStmt size -> CQPL.ProcDef size

data UnitaryCompilePrimBuilder shape size = UnitaryCompilePrimBuilder
  { mk_ucall :: shape (UCallBuilder size)
  -- ^ helper to generate a call to a unitary function argument.
  , ret_vars :: [Ident]
  }

reshapeBuilder ::
  (ValidPrimShape shape, ValidPrimShape shape') =>
  UnitaryCompilePrimBuilder shape size ->
  Either String (UnitaryCompilePrimBuilder shape' size)
reshapeBuilder UnitaryCompilePrimBuilder{..} = do
  mk_ucall' <- reshape mk_ucall
  return UnitaryCompilePrimBuilder{mk_ucall = mk_ucall', ..}

type UnitaryCompilePrimMonad ext' prim =
  RWST
    (UnitaryCompilePrimBuilder (PrimFnShape prim) (SizeType prim))
    ()
    ()
    (CompilerT ext')

-- | Compile a primitive to a unitary statement.
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  UnitaryCompilePrim prim size prec
    | prim -> size prec
  where
  compileUPrim ::
    forall ext' m shape.
    ( m ~ UnitaryCompilePrimMonad ext' prim
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    A.FailProb prec ->
    m (CQPL.ProcDef size)
  default compileUPrim ::
    forall ext' m shape.
    ( Generic prim
    , GUnitaryCompilePrim (Rep prim) size prec
    , m ~ UnitaryCompilePrimMonad ext' prim
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    A.FailProb prec ->
    m (CQPL.ProcDef size)
  compileUPrim prim eps = do
    builder <- view id
    lift $ do
      builder' <- lift $ reshapeBuilder builder
      gcompileUPrim (from prim) eps builder'

class GUnitaryCompilePrim f size prec | f -> size prec where
  gcompileUPrim ::
    forall ext' m p.
    ( m ~ CompilerT ext'
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    ) =>
    f p ->
    A.FailProb prec ->
    UnitaryCompilePrimBuilder [] size ->
    m (CQPL.ProcDef size)

instance (GUnitaryCompilePrim a size prec, GUnitaryCompilePrim b size prec) => GUnitaryCompilePrim (a :+: b) size prec where
  gcompileUPrim (L1 x) = gcompileUPrim x
  gcompileUPrim (R1 x) = gcompileUPrim x

instance (GUnitaryCompilePrim f size prec) => GUnitaryCompilePrim (M1 i c f) size prec where
  gcompileUPrim (M1 x) = gcompileUPrim x

instance (UnitaryCompilePrim a size prec) => GUnitaryCompilePrim (K1 i a) size prec where
  gcompileUPrim (K1 x) eps builder = do
    builder' <- lift $ reshapeBuilder builder
    (a, (), ()) <- runRWST (compileUPrim x eps) builder' ()
    pure a

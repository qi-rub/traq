{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Compile (
  PrimCompileEnv (..),
  UnitaryCompilePrim (..),
  QuantumCompilePrim (..),
  PrimCompileMonad,
) where

import Control.Monad.Reader (ReaderT (..))
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
-- Environment and enclosing monad for compiling primitives.
-- --------------------------------------------------------------------------------

type UCallBuilder size = [CQPL.Arg size] -> CQPL.UStmt size
type CallBuilder size = [Ident] -> CQPL.Stmt size

-- | Helpers to compile a primitive.
data PrimCompileEnv shape size = PrimCompileEnv
  { mk_ucall :: shape (UCallBuilder size)
  -- ^ helper to generate a call to a unitary function argument.
  , mk_call :: shape (CallBuilder size)
  -- ^ helper to generate a call to a classical function argument.
  , mk_meas :: shape (CallBuilder size)
  -- ^ helper to generate a call-and-meas to a unitary proc arg.
  , uproc_aux_types :: shape [P.VarType size]
  -- ^ auxiliary variables for each unitary function argument.
  , ret_vars :: [Ident]
  -- ^ return variables to store the result in.
  }

reshapeBuilder ::
  (ValidPrimShape shape, ValidPrimShape shape') =>
  PrimCompileEnv shape size ->
  Either String (PrimCompileEnv shape' size)
reshapeBuilder PrimCompileEnv{..} = do
  mk_ucall <- reshape mk_ucall
  mk_call <- reshape mk_call
  mk_meas <- reshape mk_meas
  uproc_aux_types <- reshape uproc_aux_types
  return PrimCompileEnv{..}

type PrimCompileMonad ext prim =
  ReaderT
    (PrimCompileEnv (PrimFnShape prim) (SizeType prim))
    (CompilerT ext)

-- --------------------------------------------------------------------------------
-- Unitary Compilation
-- --------------------------------------------------------------------------------

-- | Compile a primitive to a uproc
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
    ( m ~ PrimCompileMonad ext' prim
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
    , m ~ PrimCompileMonad ext' prim
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
    PrimCompileEnv [] size ->
    m (CQPL.ProcDef size)

instance (GUnitaryCompilePrim a size prec, GUnitaryCompilePrim b size prec) => GUnitaryCompilePrim (a :+: b) size prec where
  gcompileUPrim (L1 x) = gcompileUPrim x
  gcompileUPrim (R1 x) = gcompileUPrim x

instance (GUnitaryCompilePrim f size prec) => GUnitaryCompilePrim (M1 i c f) size prec where
  gcompileUPrim (M1 x) = gcompileUPrim x

instance (UnitaryCompilePrim a size prec) => GUnitaryCompilePrim (K1 i a) size prec where
  gcompileUPrim (K1 x) eps builder = do
    builder' <- lift $ reshapeBuilder builder
    runReaderT (compileUPrim x eps) builder'

-- --------------------------------------------------------------------------------
-- Quantum Compilation
-- --------------------------------------------------------------------------------

-- | Compile a primitive to a cq-proc
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  QuantumCompilePrim prim size prec
    | prim -> size prec
  where
  compileQPrim ::
    forall ext' m shape.
    ( m ~ PrimCompileMonad ext' prim
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    A.FailProb prec ->
    m (CQPL.ProcDef size)
  default compileQPrim ::
    forall ext' m shape.
    ( Generic prim
    , GQuantumCompilePrim (Rep prim) size prec
    , m ~ PrimCompileMonad ext' prim
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    A.FailProb prec ->
    m (CQPL.ProcDef size)
  compileQPrim prim eps = do
    builder <- view id
    lift $ do
      builder' <- lift $ reshapeBuilder builder
      gcompileQPrim (from prim) eps builder'

class GQuantumCompilePrim f size prec | f -> size prec where
  gcompileQPrim ::
    forall ext' m p.
    ( m ~ CompilerT ext'
    , size ~ SizeType ext'
    , prec ~ PrecType ext'
    ) =>
    f p ->
    A.FailProb prec ->
    PrimCompileEnv [] size ->
    m (CQPL.ProcDef size)

instance (GQuantumCompilePrim a size prec, GQuantumCompilePrim b size prec) => GQuantumCompilePrim (a :+: b) size prec where
  gcompileQPrim (L1 x) = gcompileQPrim x
  gcompileQPrim (R1 x) = gcompileQPrim x

instance (GQuantumCompilePrim f size prec) => GQuantumCompilePrim (M1 i c f) size prec where
  gcompileQPrim (M1 x) = gcompileQPrim x

instance (QuantumCompilePrim a size prec) => GQuantumCompilePrim (K1 i a) size prec where
  gcompileQPrim (K1 x) eps builder = do
    builder' <- lift $ reshapeBuilder builder
    runReaderT (compileQPrim x eps) builder'

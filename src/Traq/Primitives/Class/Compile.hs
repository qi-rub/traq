{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.Compile (
  UnitaryCompilePrim (..),
  PrimCompileEnv (..),
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
-- Unitary Compilation
-- --------------------------------------------------------------------------------

type UCallBuilder size = [Ident] -> CQPL.UStmt size
type CallBuilder size = [Ident] -> CQPL.Stmt size

-- type UProcBuilder size = [(Ident, P.VarType size)] -> CQPL.UStmt size -> CQPL.ProcDef size

data PrimCompileEnv shape size = PrimCompileEnv
  { mk_ucall :: shape (UCallBuilder size)
  -- ^ helper to generate a call to a unitary function argument.
  , mk_call :: shape (CallBuilder size)
  -- ^ helper to generate a call to a classical function argument.
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
  uproc_aux_types <- reshape uproc_aux_types
  return PrimCompileEnv{..}

type UnitaryCompilePrimMonad ext prim =
  ReaderT
    (PrimCompileEnv (PrimFnShape prim) (SizeType prim))
    (CompilerT ext)

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

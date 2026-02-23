{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  compileWith,

  -- ** proc builder
  ProcBuilderT,
  allocLocalWithPrefix,
  allocLocal,
  addUStmt,
  withUStmt,
  addStmt,
  withStmt,
  buildProc,

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

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Extra (loopM)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, WriterT (..), censor)
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
data LoweringCtx size
  = LoweringCtx
      -- | The set of already used identifiers (to generate unique ones)
      (Set.Set Ident)
      -- | The typing context: mapping all variables in context to their types.
      (P.TypingCtx size)
      -- | Signature of each uproc
      (Map.Map Ident (ProcSignature size))
  deriving (Generic, HasDefault)

type instance SizeType (LoweringCtx size) = size

instance HasUniqNamesCtx (LoweringCtx size) where
  _uniqNamesCtx focus (LoweringCtx a b c) = focus a <&> \a' -> LoweringCtx a' b c

instance P.HasTypingCtx (LoweringCtx size) where
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

-- | Run the given compiler on a full program.
compileWith ::
  forall ext size m.
  ( m ~ CompilerT ext
  , size ~ SizeType ext
  , P.HasFreeVars ext
  , P.TypeInferrable ext size
  ) =>
  (P.Program ext -> m ()) ->
  P.Program ext ->
  Either String (CQPL.Program size)
compileWith compiler prog = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"
  P.typeCheckProg prog

  let config =
        default_
          & (P._funCtx .~ P.programToFunCtx prog)
  let lowering_ctx =
        default_
          & (_uniqNamesCtx .~ P.allNamesP prog)

  (_, _, output) <- runRWST (compiler prog) config lowering_ctx

  return $ CQPL.Program $ output ^. _loweredProcs

-- ================================================================================
-- Helper to build procs
-- ================================================================================

-- | Transformer to build CQPL procs in the compiler.
type VarList size = [(Ident, P.VarType size)]

type ProcBuilderT size =
  WriterT
    (VarList size, [CQPL.UStmt size], [CQPL.Stmt size])

type IsProcBuilder m' size m =
  ( m' ~ ProcBuilderT size m
  , Monad m
  , MonadError String m
  , MonadState (LoweringCtx size) m
  )

allocLocalWithPrefix ::
  (IsProcBuilder m' size m) =>
  Ident ->
  P.VarType size ->
  m' Ident
allocLocalWithPrefix pref ty = do
  x <- newIdent pref
  writeElemAt _1 (x, ty)
  return x

allocLocal ::
  (IsProcBuilder m' size m) =>
  P.VarType size ->
  m' Ident
allocLocal = allocLocalWithPrefix "aux"

addUStmt :: (IsProcBuilder m' size m) => CQPL.UStmt size -> m' ()
addUStmt = writeElemAt _2

withUStmt :: (IsProcBuilder m' size m) => (CQPL.UStmt size -> CQPL.UStmt size) -> m' a -> m' a
withUStmt f = censor (_2 %~ f')
 where
  f' ss = [f (CQPL.USeqS ss)]

addStmt :: (IsProcBuilder m' size m) => CQPL.Stmt size -> m' ()
addStmt = writeElemAt _3

withStmt :: (IsProcBuilder m' size m) => (CQPL.Stmt size -> CQPL.Stmt size) -> m' a -> m' a
withStmt f = censor (_3 %~ f')
 where
  f' ss = [f (CQPL.SeqS ss)]

buildProc ::
  (IsProcBuilder m' size m) =>
  Ident ->
  [Ident] ->
  [(Ident, P.VarType size)] ->
  m' () ->
  m (CQPL.ProcDef size)
buildProc proc_name_basic proc_meta_params params m = do
  proc_name <- newIdent proc_name_basic
  ((), (local_vars, ubody, cbody)) <- runWriterT $ withSandboxOf P._typingCtx m

  case (ubody, cbody) of
    ([], []) -> throwError "buildProc: no body statements!"
    ([], _) ->
      pure $
        CQPL.ProcDef
          { info_comment = ""
          , proc_name
          , proc_meta_params
          , proc_param_types = map snd params
          , proc_body =
              CQPL.ProcBodyC
                CQPL.CProcBody
                  { cproc_param_names = map fst params
                  , cproc_local_vars = local_vars
                  , cproc_body_stmt = CQPL.SeqS cbody
                  }
          }
    (_, []) ->
      pure $
        CQPL.ProcDef
          { info_comment = ""
          , proc_name
          , proc_meta_params
          , proc_param_types = map snd params ++ map snd local_vars
          , proc_body =
              CQPL.ProcBodyU
                CQPL.UProcBody
                  { uproc_param_names = map fst params ++ map fst local_vars
                  , uproc_param_tags = replicate (length params) CQPL.ParamUnk ++ replicate (length local_vars) CQPL.ParamAux
                  , uproc_body_stmt = CQPL.USeqS ubody
                  }
          }
    _ -> throwError "buildProc: contains both ustmt and stmt"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Annotate.Prelude (
  AnnFailProb (..),

  -- * Monad
  AnnotateMonad,
  Annotater,
  annotateProgWith,

  -- ** Env
  AnnotateEnv,
  mkAnnotateEnv,

  -- ** State
  AnnotateState (..),
  _unique_id,
  nextId,
  _funs,
  addFn,
) where

import Control.Monad.RWS (RWST (..))

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Data.Default (HasDefault (..))

import Traq.Analysis.Error.Prelude
import Traq.Prelude
import Traq.ProtoLang
import qualified Traq.Utils.Printing as PP

-- ============================================================================
-- Annotate each primitive with a failure probability.
-- ============================================================================

data AnnFailProb ext = AnnFailProb (FailProb (PrecType ext)) ext

type instance SizeType (AnnFailProb ext) = SizeType ext
type instance PrecType (AnnFailProb ext) = PrecType ext

deriving instance (Show (PrecType ext), Show ext) => Show (AnnFailProb ext)

instance (PP.ToCodeString ext, Show (PrecType ext)) => (PP.ToCodeString (AnnFailProb ext)) where
  build (AnnFailProb eps ext) = do
    s <- PP.fromBuild ext
    PP.putWord $ "fail_prob[" ++ show eps ++ "] " ++ s

instance (TypeInferrable ext size) => (TypeInferrable (AnnFailProb ext) size) where
  inferTypes (AnnFailProb _ e) = inferTypes e

instance (Evaluatable ext size prec) => Evaluatable (AnnFailProb ext) size prec where
  eval (AnnFailProb _ e) = eval e

-- ============================================================================
-- RS Monad to perform annotation
-- ============================================================================
type AnnotateEnv ext = FunCtx ext

mkAnnotateEnv :: Program ext -> AnnotateEnv ext
mkAnnotateEnv (Program fs) = namedFunsToFunCtx fs

data AnnotateState ext = AnnotateSymState
  { unique_id :: Int
  , funs :: [NamedFunDef (AnnFailProb ext)]
  }

instance HasDefault (AnnotateState ext) where
  default_ = AnnotateSymState{unique_id = 0, funs = []}

_unique_id :: Lens' (AnnotateState ext) Int
_unique_id focus s = focus (unique_id s) <&> \v -> s{unique_id = v}

nextId :: (m ~ AnnotateMonad ext) => m Int
nextId = do
  i <- use _unique_id
  _unique_id += 1
  return i

_funs :: Lens' (AnnotateState ext) [NamedFunDef (AnnFailProb ext)]
_funs focus s = focus (funs s) <&> \v -> s{funs = v}

addFn :: (m ~ AnnotateMonad ext) => NamedFunDef (AnnFailProb ext) -> m ()
addFn f = _funs %= (f :)

type AnnotateMonad ext =
  RWST
    (AnnotateEnv ext)
    ()
    (AnnotateState ext)
    (Either String)

type Annotater ext = Program ext -> AnnotateMonad ext (Program (AnnFailProb ext))

annotateProgWith ::
  forall ext.
  Annotater ext ->
  Program ext ->
  Either String (Program (AnnFailProb ext))
annotateProgWith ann_act prog = do
  (prog', _, _) <- runRWST (ann_act prog) (mkAnnotateEnv prog) default_
  return prog'

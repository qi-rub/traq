{-# LANGUAGE DeriveGeneric #-}
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
  PrecisionSplittingStrategy (..),
  HasPrecisionSplittingStrategy (..),

  -- ** State
  AnnotateState (..),
  _unique_id,
  nextId,
) where

import Control.Monad.RWS (RWST (..))
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx
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

-- ----------------------------------------------------------------------------
-- Env
-- ----------------------------------------------------------------------------
data PrecisionSplittingStrategy = SplitSimple | SplitUsingNeedsEps
  deriving (Eq, Read, Show, Enum)

instance HasDefault PrecisionSplittingStrategy where
  default_ = SplitSimple

-- Predicate for checking if a type has precision splitting strategy
class HasPrecisionSplittingStrategy p where
  _precSplitStrat :: Lens' p PrecisionSplittingStrategy

data AnnotateEnv ext
  = AnnotateEnv
      (FunCtx ext)
      PrecisionSplittingStrategy
  deriving (Generic)

instance HasDefault (AnnotateEnv ext)

instance HasFunCtx (AnnotateEnv ext) ext where
  _funCtx focus (AnnotateEnv f s) = focus f <&> \f' -> AnnotateEnv f' s

instance HasPrecisionSplittingStrategy (AnnotateEnv ext) where
  _precSplitStrat focus (AnnotateEnv f s) = focus s <&> \s' -> AnnotateEnv f s'

mkAnnotateEnv :: Program ext -> AnnotateEnv ext
mkAnnotateEnv prog =
  default_
    & (_funCtx .~ programToFunCtx prog)

-- ----------------------------------------------------------------------------
-- State
-- ----------------------------------------------------------------------------
data AnnotateState ext ext' = AnnotateSymState
  { unique_id :: Int
  , funs :: Ctx.Context (FunDef ext')
  }

instance HasDefault (AnnotateState ext ext') where
  default_ = AnnotateSymState{unique_id = 0, funs = mempty}

instance HasFunCtx (AnnotateState ext ext') ext' where
  _funCtx focus s = focus (funs s) <&> \v -> s{funs = v}

_unique_id :: Lens' (AnnotateState ext ext') Int
_unique_id focus s = focus (unique_id s) <&> \v -> s{unique_id = v}

nextId :: (m ~ AnnotateMonad ext ext') => m Int
nextId = do
  i <- use _unique_id
  _unique_id += 1
  return i

-- ----------------------------------------------------------------------------
-- Monad
-- ----------------------------------------------------------------------------
type AnnotateMonad ext ext' =
  RWST
    (AnnotateEnv ext)
    ()
    (AnnotateState ext ext')
    (Either String)

type Annotater ext ext' = Program ext -> AnnotateMonad ext ext' (Program ext')

annotateProgWith ::
  forall ext.
  Annotater ext (AnnFailProb ext) ->
  Program ext ->
  Either String (Program (AnnFailProb ext))
annotateProgWith ann_act prog = do
  (prog', _, _) <- runRWST (ann_act prog) (mkAnnotateEnv prog) default_
  return prog'

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Analysis.Annotate (
  AnnFailProb (..),
) where

import Traq.Analysis.Error
import Traq.Prelude
import Traq.ProtoLang.Eval
import Traq.ProtoLang.TypeCheck

-- ============================================================================
-- Annotate each primitive with a failure probability.
-- ============================================================================

data AnnFailProb ext = AnnFailProb (FailProb (PrecType ext)) ext

type instance SizeType (AnnFailProb ext) = SizeType ext
type instance PrecType (AnnFailProb ext) = PrecType ext

instance (TypeInferrable ext size) => (TypeInferrable (AnnFailProb ext) size) where
  inferTypes (AnnFailProb _ e) = inferTypes e

instance (Evaluatable ext size prec) => Evaluatable (AnnFailProb ext) size prec where
  eval (AnnFailProb _ e) = eval e

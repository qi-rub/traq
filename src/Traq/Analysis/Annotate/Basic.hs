module Traq.Analysis.Annotate.Basic (
  annNoPrims,
  annFixedEps,
  annSinglePrim,
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Error.Prelude
import Traq.Prelude

-- | Trivially annotate a program with no primitive calls.
annNoPrims ::
  (m ~ AnnotateMonad ext (AnnFailProb ext)) =>
  ext ->
  m (AnnFailProb ext)
annNoPrims _ = throwError "annNoPrims assumes no primitive calls!"

-- | Annotate each primitive with the given epsilon
annFixedEps ::
  ( m ~ AnnotateMonad ext (AnnFailProb ext)
  , prec ~ PrecType ext
  , Num prec
  ) =>
  FailProb prec ->
  ext ->
  m (AnnFailProb ext)
annFixedEps eps p = pure $ AnnFailProb eps p

{- | Annotate the only primitive with the given epsilon,
| and raise an error if there are multiple.
-}
annSinglePrim ::
  ( m ~ AnnotateMonad ext (AnnFailProb ext)
  , prec ~ PrecType ext
  , Num prec
  ) =>
  FailProb prec ->
  ext ->
  m (AnnFailProb ext)
annSinglePrim eps p = do
  i <- nextId
  when (i /= 0) $ throwError "more than one primitive calls!"
  pure $ AnnFailProb eps p

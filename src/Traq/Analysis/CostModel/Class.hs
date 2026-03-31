module Traq.Analysis.CostModel.Class (
  QueryType (..),
  CostModel (..),
) where

import qualified Numeric.Algebra as Alg

import qualified Traq.CPL.Syntax as CPL
import Traq.Prelude
import qualified Traq.QPL.Syntax as QPL

-- | Type of a query/execution: either run on a classical computer, or a quantum computer (as a unitary).
data QueryType = Classical | Unitary
  deriving (Eq, Read, Show)

-- | A generic cost model
class (Alg.Monoidal c, Alg.Module (PrecType c) c) => CostModel c where
  -- | Make one query to a function of the given name
  query :: QueryType -> Ident -> c

  -- | Execute a classical expression.
  callExpr :: CPL.BasicExpr size -> c

  -- | Execute a classical distribution (randomized) expression
  callDistrExpr :: (prec ~ PrecType c) => CPL.DistrExpr prec size -> c

  -- | Execute a basic unitary operation of QPL
  callUOp :: (prec ~ PrecType c) => QPL.Unitary prec size -> c

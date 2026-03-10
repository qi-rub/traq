module Traq.Analysis.CostModel.Class (
  QueryType (..),
  CostModel (..),
) where

import qualified Numeric.Algebra as Alg

import qualified Traq.CPL.Syntax as P
import Traq.Prelude

-- | Type of a query/execution: either run on a classical computer, or a quantum computer (as a unitary).
data QueryType = Classical | Unitary
  deriving (Eq, Read, Show)

-- | A generic cost model
class (Alg.Monoidal c, Alg.Module (PrecType c) c) => CostModel c where
  -- | Make one query to a function of the given name
  query :: QueryType -> Ident -> c

  -- | Execute an expression.
  callExpr :: QueryType -> P.BasicExpr size -> c

  -- | Execute a distribution (randomized) expression
  callDistrExpr :: QueryType -> P.DistrExpr size -> c

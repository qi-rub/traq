module Traq.Analysis.CostModel.Class (
  QueryType (..),
  CostModel (..),
) where

import qualified Numeric.Algebra as Alg

import Traq.Prelude
import qualified Traq.ProtoLang.Syntax as P

-- | Type of a query/execution: either run on a classical computer, or a quantum computer (as a unitary).
data QueryType = Classical | Unitary
  deriving (Eq, Read, Show)

-- | A generic cost model
class (Alg.Monoidal c, Alg.Module (PrecType c) c) => CostModel c where
  -- | Make one query to a function of the given name
  query :: QueryType -> Ident -> c

  -- | Execute an expression.
  callExpr :: QueryType -> P.BasicExpr sizeT -> c

  -- | Execute a distribution (randomized) expression
  callDistrExpr :: QueryType -> P.DistrExpr sizeT -> c

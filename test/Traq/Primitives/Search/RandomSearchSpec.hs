module Traq.Primitives.Search.RandomSearchSpec (spec) where

import qualified Data.Map as Map

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import Traq.Examples.MatrixSearch (mkMatrixExample)
import Traq.Prelude
import Traq.Primitives (Primitive (Primitive))
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.RandomSearch

import Test.Hspec

diagMatrix :: [CPL.Value SizeT] -> [CPL.Value SizeT]
diagMatrix [CPL.FinV i, CPL.FinV j] = [CPL.toValue $ i < j]
diagMatrix _ = error "invalid input"

spec :: Spec
spec = do
  describe "RandomSearch" $ do
    let mat_prog = mkMatrixExample (\ty f -> CPL.PrimCallE $ Primitive [f] (RandomSearch $ PrimSearch AnyK ty))
    it "expected cost" $ do
      let eps = A.failProb 0.001
      let n = 10
      let fun_interp = Map.singleton "Matrix" diagMatrix
      prog' <- either fail pure $ A.annotateProgWithErrorBudget eps $ mat_prog n n
      let c = getCost $ A.expCostQProg prog' [] fun_interp :: Double
      c `shouldSatisfy` (< (292 :: Double))

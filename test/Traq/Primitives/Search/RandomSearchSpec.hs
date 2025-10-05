{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.RandomSearchSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Examples.MatrixSearch (matrixExample)
import Traq.Prelude
import Traq.Primitives.Search.RandomSearch
import qualified Traq.ProtoLang as P

import Test.Hspec

diagMatrix :: [P.Value SizeT] -> [P.Value SizeT]
diagMatrix [P.FinV i, P.FinV j] = [P.toValue $ i < j]
diagMatrix _ = error "invalid input"

spec :: Spec
spec = do
  describe "RandomSearch" $ do
    let mat_prog = matrixExample @RandomSearch
    it "expected cost" $ do
      let eps = 0.001
      let n = 10
      let fun_interp = Ctx.singleton "Matrix" diagMatrix
      let c = getCost $ P.quantumQueryCost default_ eps (mat_prog n n) fun_interp default_ :: Double
      c `shouldSatisfy` (< (292 :: Double))

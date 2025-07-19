{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.RandomSearchSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import Traq.Prelude

import qualified Traq.ProtoLang as P

import Traq.Examples.MatrixSearch (matrixExample)
import Traq.Primitives.Search.RandomSearch

import Test.Hspec

diagMatrix :: [P.Value SizeT] -> [P.Value SizeT]
diagMatrix [P.FinV i, P.FinV j] = [P.boolToValue $ i < j]
diagMatrix _ = error "invalid input"

spec :: Spec
spec = do
  describe "RandomSearch" $ do
    let mat_prog n m = matrixExample @RandomSearch n m (P.Fin (2 :: Int))
    it "expected cost" $ do
      let eps = 0.001
      let n = 10
      let ticks = mempty & at "Oracle" ?~ 1.0
      let fun_interp = Ctx.singleton "Oracle" diagMatrix
      let c = P.quantumQueryCost default_ eps (mat_prog n n) ticks ticks fun_interp default_
      c `shouldSatisfy` (< (292 :: Double))

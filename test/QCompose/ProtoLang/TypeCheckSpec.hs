module QCompose.ProtoLang.TypeCheckSpec (spec) where

import qualified Data.Map as M
import QCompose.Examples.MatrixSearch (matrixExampleS)
import QCompose.ProtoLang.TypeCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "matrix example" $ do
      isWellTyped M.empty (matrixExampleS 4 5) `shouldBe` True

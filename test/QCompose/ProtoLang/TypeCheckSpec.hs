module QCompose.ProtoLang.TypeCheckSpec (spec) where

import QCompose.Examples.MatrixSearch (matrixExampleS)
import QCompose.ProtoLang.TypeCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "matrix example" $ do
      isWellTyped (matrixExampleS 4 5) `shouldBe` True

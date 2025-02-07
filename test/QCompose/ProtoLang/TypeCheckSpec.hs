module QCompose.ProtoLang.TypeCheckSpec (spec) where

import QCompose.Examples.MatrixSearch (matrixExample)
import QCompose.ProtoLang.TypeCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "matrix example" $ do
      isWellTyped (matrixExample 4 5) `shouldBe` True

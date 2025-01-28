module QCompose.TypeCheckSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
	describe "typecheck" $ do
		it "does nothing" $ do
			0 `shouldBe` 0

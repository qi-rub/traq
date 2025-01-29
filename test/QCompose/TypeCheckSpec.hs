module QCompose.TypeCheckSpec (spec) where

import Test.Hspec
import QCompose.ProtoLang
import QCompose.TypeCheck

spec :: Spec
spec = do
	describe "typecheck" $ do
		it "does nothing" $ do
			0 `shouldBe` 0

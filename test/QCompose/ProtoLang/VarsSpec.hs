module QCompose.ProtoLang.VarsSpec (spec) where

import qualified Data.Set as S
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.Vars
import Test.Hspec

spec :: Spec
spec = do
  describe "vars" $ do
    it "inputs" $ do
      freeVars (ExprS{rets = ["x"], expr = VarE{arg = "y"}}) `shouldBe` S.fromList ["y"]

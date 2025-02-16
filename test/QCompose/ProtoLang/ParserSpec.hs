module QCompose.ProtoLang.ParserSpec (spec) where

import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      parseCode "x' <- x" `shouldBe` Right SAssign{ret = "x'", arg = "x"}
    it "parses seq assign" $ do
      parseCode "x' <- x; y' <- const 3 : Fin<4>"
        `shouldBe` Right
          ( SSeq
              SAssign{ret = "x'", arg = "x"}
              SConst{ret = "y'", val = 3, ty = Fin (Right 4)}
          )

module QCompose.ProtoLang.RewritesSpec (spec) where

import QCompose.ProtoLang.Rewrites
import QCompose.ProtoLang.Syntax
import QCompose.Utils.ASTRewriting

import Test.Hspec

spec :: Spec
spec = do
  describe "flattenSeq" $ do
    it "flattens simple" $ do
      let s = ExprS{rets = ["x"], expr = VarE{arg = "y"}} :: Stmt Int
      rewriteAST flattenSeq s `shouldBe` s
      rewriteAST flattenSeq (SeqS [s]) `shouldBe` s
    it "flattens skip" $ do
      let skip = SeqS [] :: Stmt Int
      rewriteAST flattenSeq skip `shouldBe` skip
      rewriteAST flattenSeq (SeqS [skip]) `shouldBe` skip

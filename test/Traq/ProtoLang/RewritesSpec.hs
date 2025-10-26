module Traq.ProtoLang.RewritesSpec (spec) where

import Traq.ProtoLang.Lenses ()
import Traq.ProtoLang.Rewrites
import Traq.ProtoLang.Syntax
import Traq.Utils.ASTRewriting

import Test.Hspec

spec :: Spec
spec = do
  describe "flattenSeq" $ do
    it "flattens simple" $ do
      let s = ExprS{rets = ["x"], expr = BasicExprE VarE{var = "y"}} :: Stmt Core'
      rewriteAST flattenSeq s `shouldBe` s
      rewriteAST flattenSeq (SeqS [s]) `shouldBe` s
    it "flattens skip" $ do
      let skip = SeqS [] :: Stmt Core'
      rewriteAST flattenSeq skip `shouldBe` skip
      rewriteAST flattenSeq (SeqS [skip]) `shouldBe` skip

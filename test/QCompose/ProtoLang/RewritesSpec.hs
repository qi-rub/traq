module QCompose.ProtoLang.RewritesSpec (spec) where

import Lens.Micro
import QCompose.ProtoLang.Rewrites
import QCompose.ProtoLang.Syntax
import Test.Hspec

-- | statement rewriter
rw :: (Stmt a -> Maybe (Stmt a)) -> Stmt a -> Stmt a
rw = rewriteOf _ast

spec :: Spec
spec = do
  describe "flattenSeq" $ do
    it "flattens simple" $ do
      let s = AssignS{ret = "x", arg = "y"} :: Stmt Int
      rw flattenSeq s `shouldBe` s
      rw flattenSeq (SeqS [s]) `shouldBe` s
    it "flattens skip" $ do
      let skip = SeqS [] :: Stmt Int
      rw flattenSeq skip `shouldBe` skip
      rw flattenSeq (SeqS [skip]) `shouldBe` skip

module QCompose.ProtoLang.RewritesSpec (spec) where

import Lens.Micro (rewriteOf)
import QCompose.ProtoLang.Rewrites
import QCompose.ProtoLang.Syntax
import Test.Hspec

spec :: Spec
spec = do
  return ()

-- describe "flattenSeq" $ do
--   it "flattens simple" $ do
--   let s = AssignS{ret = "x", arg = "y"} :: Stmt Int
--   _stmt flattenSeq s `shouldBe` Nothing
--   _stmt flattenSeq (SeqS [s]) `shouldBe` Just s
-- it "flattens skip" $ do
--   let skip = SeqS [] :: Stmt Int
--   _stmt flattenSeq skip `shouldBe` Nothing
--   _stmt flattenSeq (SeqS [skip]) `shouldBe` Just skip

-- rewriteOf _stmt flattenSeq (SeqS [s]) `shouldBe` s

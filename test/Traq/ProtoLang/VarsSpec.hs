{-# LANGUAGE OverloadedStrings #-}

module Traq.ProtoLang.VarsSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.Vars

spec :: Spec
spec = do
  describe "vars" $ do
    it "inputs" $ do
      freeVars (ExprS{rets = ["x"], expr = BasicExprE "y"}) `shouldBe` Set.fromList ["y"]

{-# LANGUAGE OverloadedStrings #-}

module Traq.ProtoLang.VarsSpec (spec) where

import qualified Data.Set as Set
import Data.Void (Void)

import Traq.ProtoLang.Syntax
import Traq.ProtoLang.Vars

import Test.Hspec

spec :: Spec
spec = do
  describe "vars" $ do
    it "inputs" $ do
      freeVars (ExprS{rets = ["x"], expr = BasicExprE "y"} :: Stmt Void Int) `shouldBe` Set.fromList ["y"]

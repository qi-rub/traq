{-# LANGUAGE OverloadedStrings #-}

module Traq.CPL.VarsSpec (spec) where

import qualified Data.Set as Set

import Traq.CPL.Syntax
import Traq.CPL.Vars

import Test.Hspec

spec :: Spec
spec = do
  describe "vars" $ do
    it "inputs" $ do
      freeVars (ExprS{rets = ["x"], expr = BasicExprE "y"} :: Stmt Core') `shouldBe` Set.fromList ["y"]

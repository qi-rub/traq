module Traq.Data.SymbolicSpec (spec) where

import qualified Traq.Data.Symbolic as Sym

import Test.Hspec

spec :: Spec
spec = describe "Data.Symbolic" $ do
  it "construct" $ do
    let n = 10 :: Int
    Sym.con n + Sym.con n `shouldBe` Sym.con (2 * n)

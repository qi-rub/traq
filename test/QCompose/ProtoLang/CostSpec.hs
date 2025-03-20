module QCompose.ProtoLang.CostSpec (spec) where

import qualified Data.Number.Symbolic as Sym

import QCompose.Prelude
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Syntax
import QCompose.Primitives.QSearch

import Test.Hspec

unsafeParseProgram :: String -> Program SizeT
unsafeParseProgram = fmap Sym.unSym . either (error . show) id . parseProgram

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    it "fun call of oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "def f(i : Fin<100>) do"
              , "  b <- Oracle(i);"
              , "  return b : Fin<2>"
              , "end"
              , "i <- const 10 : Fin<100>;"
              , "res <- f(i)"
              ]
      let c = unitaryQueryCost cadeEtAlFormulas 0.001 prog
      c `shouldBe` 1

    it "search with no oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "def f(i : Fin<100>) do"
              , "  b <- const 0 : Fin<2>;"
              , "  return b : Fin<2>"
              , "end"
              , "res <- any(f)"
              ]
      let c = unitaryQueryCost cadeEtAlFormulas 0.001 prog
      c `shouldBe` 0

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "def f(i : Fin<100>) do"
              , "  b <- Oracle(i);"
              , "  return b : Fin<2>"
              , "end"
              , "res <- any(f)"
              ]
      let c = unitaryQueryCost cadeEtAlFormulas 0.001 prog
      c `shouldBe` 2 * qSearchUnitaryCost cadeEtAlFormulas 100 (0.001 / 2)

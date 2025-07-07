module Traq.ProtoLang.CostSpec (spec) where

import qualified Data.Map as Map

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.ProtoLang.Cost
import Traq.ProtoLang.Parser
import Traq.ProtoLang.Syntax

import Traq.Primitives
import Traq.Primitives.Search.QSearchCFNW (_QSearchZalka)

import Test.Hspec

unsafeParseProgram :: String -> Program DefaultPrims SizeT
unsafeParseProgram = fmap Sym.unSym . either (error . show) id . parseProgram

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    it "call oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "i <- const 10 : Fin<100>;"
              , "res <- Oracle(i)"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (2 :: Double)
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
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (4 :: Double)

    it "search with no oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "def f(i : Fin<100>) do"
              , "  b <- const 0 : Fin<2>;"
              , "  return b : Fin<2>"
              , "end"
              , "res <- @any[f]()"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (0 :: Double)

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "res <- @any[Oracle]()"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      (c :: Double) `shouldBe` 2 * _QSearchZalka (100 :: Int) (0.001 / 2)

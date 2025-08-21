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
import Traq.Data.Default
import Prelude

unsafeParseProgram :: String -> Program DefaultPrims SizeT
unsafeParseProgram = fmap Sym.unSym . either (error . show) id . parseProgram

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    it "call oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "i <- const 10 : Fin<100>;"
              , "res <- Oracle(i);"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (2 :: Double)

    it "fun call of oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def f(i : Fin<100>) -> Bool do"
              , "  b <- Oracle(i);"
              , "  return b"
              , "end"
              , "i <- const 10 : Fin<100>;"
              , "res <- f(i);"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (4 :: Double)

    it "search with no oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def f(i : Fin<100>) -> Bool do"
              , "  b <- const 0 : Fin<2>;"
              , "  return b"
              , "end"
              , "res <- @any[f]();"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      c `shouldBe` (0 :: Double)

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "res <- @any[Oracle]();"
              ]
      let c = unitaryQueryCost SplitSimple 0.001 prog (Map.singleton "Oracle" 1.0)
      (c :: Double) `shouldBe` 2 * _QSearchZalka (100 :: Int) (0.001 / 2)

    it "probabilistic outcome" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "res1 <-$ uniform Fin<2>;"
              ]
      let c = quantumQueryCost SplitSimple 0.001 prog default_ default_ default_ default_
      (c :: Double) `shouldBe` 0

module QCompose.ProtoLang.CostSpec (spec) where

import Lens.Micro
import qualified QCompose.Data.Symbolic as Sym

import QCompose.Prelude
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Syntax

import QCompose.Primitives
import QCompose.Primitives.QSearch
import QCompose.Primitives.Search.Prelude

import Test.Hspec

unsafeParseProgram :: String -> Program DefaultPrims SizeT
unsafeParseProgram = fmap Sym.unSym . either (error . show) id . parseProgram

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    let ucFormula = qsearchCFNW ^. to formulas . to qSearchUnitaryCost

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
      let c = unitaryQueryCost 0.001 prog "Oracle"
      c `shouldBe` (2 :: Double)

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
      let c = unitaryQueryCost 0.001 prog "Oracle"
      c `shouldBe` (0 :: Double)

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool"
              , "def f(i : Fin<100>) do"
              , "  b <- Oracle(i);"
              , "  return b : Fin<2>"
              , "end"
              , "res <- @any[f]()"
              ]
      let c = unitaryQueryCost 0.001 prog "Oracle"
      (c :: Double) `shouldBe` 2 * ucFormula 100 (0.001 / 2)

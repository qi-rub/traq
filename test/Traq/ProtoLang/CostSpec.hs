module Traq.ProtoLang.CostSpec (spec) where

import Prelude

import qualified Data.Map as Map

import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import Traq.CostModel.QueryCost
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.QSearchCFNW (_QSearchZalka)
import Traq.ProtoLang

import Test.Hspec

unsafeParseProgram :: String -> Program DefaultPrims SizeT
unsafeParseProgram = fmap Sym.unSym . either (error . show) id . parseProgram

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    let delta = l2NormError 0.001 :: L2NormError Double
    let eps = failProb 0.001 :: FailProb Double

    it "call oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def main() -> Bool do"
              , "i <- const 10 : Fin<100>;"
              , "res <- Oracle(i);"
              , "return res"
              , "end"
              ]
      let c = unitaryQueryCost SplitSimple delta prog :: QueryCost Double
      c `shouldBe` default_{uqueries = Map.singleton "Oracle" 2}

    it "fun call of oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def f(i : Fin<100>) -> Bool do"
              , "  b <- Oracle(i);"
              , "  return b"
              , "end"
              , "def main() -> Bool do"
              , "  i <- const 10 : Fin<100>;"
              , "  res <- f(i);"
              , "  return res"
              , "end"
              ]
      let c = unitaryQueryCost SplitSimple delta prog :: QueryCost Double
      c `shouldBe` default_{uqueries = Map.singleton "Oracle" 4}

    it "search with no oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def f(i : Fin<100>) -> Bool do"
              , "  b <- const 0 : Fin<2>;"
              , "  return b"
              , "end"
              , "def main() -> Bool do"
              , "  res <- @any[f]();"
              , "  return res"
              , "end"
              ]
      let c = unitaryQueryCost SplitSimple delta prog :: QueryCost Double
      c `shouldBe` default_

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def main() -> Bool do"
              , "  res <- @any[Oracle]();"
              , "  return res"
              , "end"
              ]
      let c = unitaryQueryCost SplitSimple delta prog :: QueryCost Double
      let qry = 2 * _QSearchZalka (100 :: Int) (delta `divideError` 2)
      c `shouldBe` default_{uqueries = Map.singleton "Oracle" qry}

    it "probabilistic outcome" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def main() -> Bool do"
              , "  res1 <-$ uniform : Fin<2>;"
              , "  return res"
              , "end"
              ]
      let c = quantumQueryCost SplitSimple eps prog default_ [] :: QueryCost Double
      c `shouldBe` default_

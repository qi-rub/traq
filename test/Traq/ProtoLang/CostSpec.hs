{-# LANGUAGE TypeApplications #-}

module Traq.ProtoLang.CostSpec (spec) where

import Prelude

import qualified Data.Map as Map

import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.QSearchCFNW (_QSearchZalka)
import Traq.ProtoLang

import Test.Hspec
import TestHelpers

unsafeParseProgram :: String -> Program DefaultPrims'
unsafeParseProgram = mapSize Sym.unSym . either (error . show) id . (parseProgram @(DefaultPrims (Sym.Sym SizeT) Double))

spec :: Spec
spec = do
  describe "unitary cost of statements" $ do
    let eps = A.failProb 0.001 :: A.FailProb Double

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
      prog' <- expectRight $ A.annotateProgWith (_exts A.annNoPrims) prog
      let c = A.costUProg prog' :: QueryCost Double
      c `shouldBe` default_{uqueries = Map.singleton "Oracle" 1}

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
      prog' <- expectRight $ A.annotateProgWith (_exts A.annNoPrims) prog
      let c = A.costUProg prog' :: QueryCost Double
      c `shouldBe` default_{uqueries = Map.singleton "Oracle" 1}

    it "search with no oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def f(i : Fin<100>) -> Bool do"
              , "  b <- const 0 : Fin<2>;"
              , "  return b"
              , "end"
              , "def main() -> Bool do"
              , "  res <- @any<Fin<100>>[f(_)];"
              , "  return res"
              , "end"
              ]
      prog' <- expectRight $ A.annotateProgWith (_exts (A.annSinglePrim eps)) prog
      let c = A.costUProg prog' :: QueryCost Double
      c `shouldBe` default_

    it "search with 1x oracle" $ do
      let prog =
            unsafeParseProgram . unlines $
              [ "declare Oracle(Fin<100>) -> Bool end"
              , "def main() -> Bool do"
              , "  res <- @any<Fin<100>>[Oracle(_)];"
              , "  return res"
              , "end"
              ]
      prog' <- expectRight $ A.annotateProgWith (_exts (A.annSinglePrim eps)) prog
      let c = A.costUProg prog' :: QueryCost Double
      let qry = 2 * _QSearchZalka (100 :: Int) eps
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
      prog' <- expectRight $ A.annotateProgWith (_exts A.annNoPrims) prog
      let c = A.costQProg prog' :: QueryCost Double
      c `shouldBe` default_

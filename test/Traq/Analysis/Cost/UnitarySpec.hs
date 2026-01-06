{-# LANGUAGE TypeApplications #-}

module Traq.Analysis.Cost.UnitarySpec (spec, unsafeParseProgram) where

import Prelude

import Data.Bifunctor (first)
import qualified Data.Map as Map

import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import Traq.Analysis
import Traq.Analysis.CostModel.QueryCost
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.QSearchCFNW (_QSearchZalka)
import Traq.ProtoLang

import Test.Hspec
import TestHelpers

unsafeParseProgram :: String -> Either String (Program DefaultPrims')
unsafeParseProgram code = do
  prog <- first show $ parseProgram @(DefaultPrims (Sym.Sym SizeT) Double) code
  pure $ mapSize Sym.unSym prog

spec :: Spec
spec = describe "unitary cost of statements" $ do
  let eps = failProb 0.001 :: FailProb Double

  it "call oracle" $ do
    prog <-
      expectRight $
        unsafeParseProgram . unlines $
          [ "declare Oracle(Fin<100>) -> Bool end"
          , "def main() -> Bool do"
          , "i <- const 10 : Fin<100>;"
          , "res <- Oracle(i);"
          , "return res"
          , "end"
          ]
    prog' <- expectRight $ annotateProgWith (_exts annNoPrims) prog
    let c = costUProg prog' :: QueryCost Double
    c `shouldBe` default_{uqueries = Map.singleton "Oracle" 1}

  it "fun call of oracle" $ do
    prog <-
      expectRight $
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
    prog' <- expectRight $ annotateProgWith (_exts annNoPrims) prog
    let c = costUProg prog' :: QueryCost Double
    c `shouldBe` default_{uqueries = Map.singleton "Oracle" 1}

  it "search with no oracle" $ do
    prog <-
      expectRight $
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
    prog' <- expectRight $ annotateProgWith (_exts (annFixedEps eps)) prog
    let c = costUProg prog' :: QueryCost Double
    c `shouldBe` default_

  it "search with 1x oracle" $ do
    prog <-
      expectRight $
        unsafeParseProgram . unlines $
          [ "declare Oracle(Fin<100>) -> Bool end"
          , "def main() -> Bool do"
          , "  res <- @any<Fin<100>>[Oracle(_)];"
          , "  return res"
          , "end"
          ]
    prog' <- expectRight $ annotateProgWith (_exts (annFixedEps eps)) prog
    let c = costUProg prog' :: QueryCost Double
    let qry = 2 * _QSearchZalka (100 :: Int) eps
    c `shouldBe` default_{uqueries = Map.singleton "Oracle" qry}

module QCompose.Examples.SearchSpec (spec) where

import Control.Monad.State (execStateT)
import qualified Data.Map as Map

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing
import QCompose.Utils.Tree

import QCompose.Examples.Search
import QCompose.Primitives.QSearch

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "arraySearch: no solutions" $ do
    let n = 10
    let ex = arraySearch n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Map.empty ex

    let oracleF = const [0]
    it "evaluates" $ do
      let res = execStateT (P.execProgram ex oracleF) Map.empty
      res `shouldBe` pure (Map.singleton "result" 0)

    let P.QSearchFormulas ecF _ ucF = cadeEtAlFormulas
    let eps = 0.0001

    it "unitary cost for eps=0.0001" $ do
      P.unitaryQueryCost cadeEtAlFormulas eps ex `shouldBe` 2 * ucF n (eps / 2)

    it "quantum cost for eps=0.0001" $ do
      P.quantumQueryCost cadeEtAlFormulas eps ex oracleF Map.empty `shouldBe` ecF n 0 (eps / 2)

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    describe "lowers to UQPL" $ do
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram zalkaQSearch Map.empty 0.001 ex

      it "typechecks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram zalkaQSearch Map.empty 0.001 ex
        assertRight $ UQPL.typeCheckProgram gamma ex_uqpl

  describe "arraySearch (returning solution)" $ do
    let n = 10
    let ex = arraySearchIx n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Map.empty ex

    let planted_sols = [2, 4, 5] :: [Value]
    let oracleF = \[i] -> [if i `elem` planted_sols then 1 else 0]

    it "evaluates" $ do
      let res = execStateT (P.execProgram ex oracleF) Map.empty
      res
        `shouldBe` choice
          [ pure $ Map.fromList [("result", 1), ("solution", i)]
          | i <- planted_sols
          ]

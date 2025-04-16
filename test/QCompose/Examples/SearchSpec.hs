module QCompose.Examples.SearchSpec (spec) where

import Lens.Micro
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Examples.Search
import QCompose.Primitives.QSearch
import QCompose.Primitives.Search.Prelude

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "arraySearch: no solutions" $ do
    let n = 10
    let ex = arraySearch n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty ex

    let oracleF = const [0]
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram ex interpCtx Ctx.empty
      res `shouldBe` pure (Ctx.singleton "result" 0)

    let qformulas@(P.QSearchFormulas ecF _ ucF) = qsearchCFNW ^. to formulas
    let ualgo = qsearchCFNW ^. to unitaryAlgo

    let eps = 0.0001 :: Double

    it "unitary cost for eps=0.0001" $ do
      let true_cost = ucF n (eps / 2) :: Double
      P.unitaryQueryCost qformulas eps ex "Oracle" `shouldBe` true_cost

    it "quantum cost for eps=0.0001" $ do
      let true_cost = ecF n 0 (eps / 2)
      P.quantumQueryCost qformulas eps ex "Oracle" interpCtx Ctx.empty `shouldBe` true_cost

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    describe "lowers to UQPL" $ do
      let delta = 0.0001 :: Double
      it "lowers" $ do
        assertRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex

      it "typechecks" $ do
        (ex_uqpl, gamma) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        assertRight $ UQPL.typeCheckProgram gamma ex_uqpl

      it "preserves cost" $ do
        (ex_uqpl, _) <- expectRight $ UQPL.lowerProgram ualgo Ctx.empty "Oracle" delta ex
        let (uqpl_cost, _) = UQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost qformulas delta ex "Oracle"
        uqpl_cost `shouldBe` proto_cost

  describe "arraySearch (returning solution)" $ do
    let n = 10
    let ex = arraySearchIx n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty ex

    let planted_sols = [2, 4, 5] :: [Value]
    let oracleF = \[i] -> [if i `elem` planted_sols then 1 else 0]
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram ex interpCtx Ctx.empty
      res
        `shouldBe` Tree.choice
          [ pure $ Ctx.fromList [("result", 1), ("solution", i)]
          | i <- planted_sols
          ]

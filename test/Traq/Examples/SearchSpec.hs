module Traq.Examples.SearchSpec (spec) where

import qualified Data.Map as Map

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Probability as Prob

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Examples.Search
import Traq.Primitives.Search.QSearchCFNW (_EQSearch, _QSearchZalka)

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "arraySearch: no solutions" $ do
    let n = 6
    let ex = arraySearch n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty ex

    let oracleF = const [P.FinV 0]
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram ex interpCtx Ctx.empty
      res `shouldBe` pure (Ctx.singleton "result" (P.FinV 0))

    let ecF = _EQSearch
    let ucF = _QSearchZalka

    let eps = 0.0001 :: Double
    let uticks = Map.singleton "Oracle" 1.0
    let ticks = Map.singleton "Oracle" 1.0

    it "unitary cost for eps=0.0001" $ do
      let true_cost = 2 * ucF n (eps / 2) :: Double
      P.unitaryQueryCost P.SplitSimple eps ex uticks `shouldBe` true_cost

    it "quantum cost for eps=0.0001" $ do
      let true_cost = 2 * ecF n 0 (eps / 2)
      P.quantumQueryCost P.SplitSimple eps ex uticks ticks interpCtx Ctx.empty `shouldBe` true_cost

    it "generate code" $ do
      PP.toCodeString ex `shouldSatisfy` (not . null)

    describe "Unitary Compile" $ do
      let delta = 0.0001 :: Double
      it "lowers" $ do
        assertRight $ CompileU.lowerProgram default_ default_ uticks delta ex

      it "typechecks" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty uticks delta ex
        assertRight $ CQPL.typeCheckProgram ex_uqpl

      it "preserves cost" $ do
        ex_uqpl <- expectRight $ CompileU.lowerProgram default_ Ctx.empty uticks delta ex
        let (uqpl_cost, _) = CQPL.programCost ex_uqpl
        let proto_cost = P.unitaryQueryCost P.SplitSimple delta ex uticks
        uqpl_cost `shouldSatisfy` (<= proto_cost)

  describe "arraySearch (returning solution)" $ do
    let n = 10
    let ex = arraySearchIx n

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty ex

    let planted_sols = [2, 4, 5] :: [SizeT]
    let oracleF = \[P.FinV i] -> [P.toValue $ i `elem` planted_sols]
    let interpCtx = Ctx.singleton "Oracle" oracleF

    it "evaluates" $ do
      let res = P.runProgram ex interpCtx Ctx.empty
      res
        `shouldBe` Prob.uniform
          [ Ctx.fromList [("result", P.FinV 1), ("solution", P.FinV i)]
          | i <- planted_sols
          ]

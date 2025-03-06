module QCompose.Examples.SearchSpec (spec) where

import Control.Monad.State (execStateT)
import Data.Either (isRight)
import qualified Data.Map as M
import QCompose.Examples.Search
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax ()
import QCompose.ProtoLang.TypeCheck
import QCompose.Subroutines.QSearch
import QCompose.UnitaryQPL.Lowering
import QCompose.Utils.Printing
import Test.Hspec

spec :: Spec
spec = do
  describe "arraySearch: no solutions" $ do
    let n = 10
    let ex = arraySearch n

    it "type checks" $ do
      isWellTyped M.empty ex `shouldBe` True

    let oracleF = const [0]
    it "evaluates" $ do
      let res = execStateT (evalProgram ex oracleF) M.empty
      res `shouldBe` Right (M.singleton "result" 0)

    let QSearchFormulas ecF _ ucF = cadeEtAlFormulas
    let eps = 0.0001

    it "unitary cost for eps=0.0001" $ do
      unitaryQueryCost cadeEtAlFormulas eps ex `shouldBe` 2 * ucF n (eps / 2)

    it "quantum cost for eps=0.0001" $ do
      quantumQueryCost cadeEtAlFormulas eps ex oracleF M.empty `shouldBe` ecF n 0 (eps / 2)

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    xit "lowers to UQPL" $ do
      let ex_uqpl = lowerProgramU M.empty 0.001 ex
      ex_uqpl `shouldSatisfy` isRight

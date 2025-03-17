module QCompose.Examples.MatrixSearchSpec (spec) where

import Control.Monad.State (execStateT)
import Data.Either (isRight)
import qualified Data.Map as M

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Examples.MatrixSearch
import QCompose.Subroutines.QSearch

import Test.Hspec

spec :: Spec
spec = do
  describe "matrix search example" $ do
    let (n, m) = (10, 10)
    let ex = matrixExampleS n m

    it "type checks" $ do
      P.isWellTyped M.empty ex `shouldBe` True

    it "has unique vars" $ do
      P.checkVarsUnique ex `shouldBe` True

    let oracleF = \[i, j] -> [if i == j then 1 else 0]
    it "evaluates" $ do
      let res = execStateT (P.execProgram ex oracleF) M.empty
      res `shouldBe` pure (M.singleton "result" 0)

    -- expected, worst, unitary
    let P.QSearchFormulas _ wcF ucF = cadeEtAlFormulas

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001
      let cu = P.unitaryQueryCost cadeEtAlFormulas delta ex
      let nu_outer = 2 * ucF n (delta / 2)
      let nu_inner = 2 * ucF m (delta / 2 / nu_outer / 2 / 2)
      cu `shouldBe` nu_outer * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = P.quantumQueryCost cadeEtAlFormulas eps ex oracleF M.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = 2 * ucF m (eps / 2 / nq_outer / 2 / 4)
      cq `shouldBe` nq_outer * nq_inner

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    xit "lowers to UQPL" $ do
      let ex_uqpl = UQPL.lowerProgram M.empty 0.001 ex
      ex_uqpl `shouldSatisfy` isRight

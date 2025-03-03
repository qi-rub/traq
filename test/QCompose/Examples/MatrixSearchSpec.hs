module QCompose.Examples.MatrixSearchSpec (spec) where

import Control.Monad.State (execStateT)
import qualified Data.Map as M
import QCompose.Examples.MatrixSearch
import QCompose.ProtoLang.Cost
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax ()
import QCompose.ProtoLang.TypeCheck
import QCompose.UnitaryQPL.Lowering
import QCompose.Utils.Printing
import Test.Hspec

spec :: Spec
spec = do
  describe "matrix search example" $ do
    let (n, m) = (10, 10)
    let ex = matrixExampleS n m

    it "type checks" $ do
      isWellTyped ex `shouldBe` True

    let oracleF = \[i, j] -> [if i == j then 1 else 0]
    it "evaluates" $ do
      let res = execStateT (evalProgram ex oracleF) M.empty
      res `shouldBe` Right (M.singleton "result" 0)

    -- expected, worst, unitary
    let QSearchFormulas _ wcF ucF = cadeEtAlFormulas

    it "unitary cost for delta=0.0001" $ do
      let delta = 0.0001
      let cu = unitaryQueryCost cadeEtAlFormulas delta ex
      let nu_outer = 2 * ucF n (delta / 2)
      let nu_inner = 2 * ucF m (delta / 2 / nu_outer / 2 / 2)
      cu `shouldBe` nu_outer * nu_inner

    it "quantum cost for eps=0.0001" $ do
      let eps = 0.0001
      let cq = quantumQueryCost cadeEtAlFormulas eps ex oracleF M.empty
      let nq_outer = wcF n (eps / 2)
      let nq_inner = ucF m (eps / 2 / nq_outer / 2 / 2)
      cq `shouldBe` nq_outer * 2 * nq_inner

    it "generate code" $ do
      toCodeString ex `shouldSatisfy` (not . null)

    it "lowers to UQPL" $ do
      pendingWith "lowering not yet implemented"
      let ex_uqpl = lowerProgramU 0.001 ex
      return ()

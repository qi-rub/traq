module QCompose.Primitives.QSearchSpec (spec) where

import Lens.Micro

import QCompose.ProtoLang (VarType (..))
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives.QSearch

import Test.Hspec

spec :: Spec
spec = do
  xdescribe "Grover circuit" $ do
    -- it "simple k=2" $ do
    --   let k = 2
    --   let n = 10 :: Int
    --   let eps = 0.001 :: Float
    --   let predicate x b = UQPL.UnitaryS [x, b] UQPL.Oracle
    --   let circ = groverK k ("x", Fin n) "b" predicate
    --   toCodeString circ `shouldSatisfy` (not . null)

    xdescribe "QSearch_Zalka circuit" $ do
      it "for simple values" $ do
        let n = 10 :: Int
        let eps = 0.001 :: Float
        let pred_caller x b = UQPL.CallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [x, b]}
        let circ = zalkaQSearchImpl (Fin n) pred_caller eps
        toCodeString circ `shouldSatisfy` (not . null)

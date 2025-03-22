module QCompose.ProtoLang.TypeCheckSpec (spec) where

import qualified Data.Map as Map
import QCompose.Examples.MatrixSearch (matrixExampleS)
import QCompose.ProtoLang.TypeCheck

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "matrix example" $ do
      assertRight $ typeCheckProg Map.empty (matrixExampleS 4 5)

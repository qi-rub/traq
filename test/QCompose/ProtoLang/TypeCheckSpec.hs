module QCompose.ProtoLang.TypeCheckSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map as Map

import QCompose.Examples.MatrixSearch (matrixExampleS)
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.TypeCheck

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "fun cannot return param" $ do
      let bad_fun =
            FunDef
              { fun_name = ""
              , param_binds = [("x", Fin 5)]
              , ret_binds = [("x", Fin 5)]
              , body = SeqS []
              } ::
              FunDef Int
      typeCheckFun undefined bad_fun `shouldSatisfy` isLeft
    it "matrix example" $ do
      assertRight $ typeCheckProg Map.empty (matrixExampleS 4 5)

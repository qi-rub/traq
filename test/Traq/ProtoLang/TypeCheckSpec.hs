module Traq.ProtoLang.TypeCheckSpec (spec) where

import Data.Either (isLeft)

import Traq.Examples.MatrixSearch (matrixExampleS)
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "fun cannot return param" $ do
      let bad_fun =
            FunDef
              { param_types = [Fin 5]
              , ret_types = [Fin 5]
              , mbody = Just FunBody{param_names = ["x"], ret_names = ["x"], body_stmt = SeqS []}
              } ::
              FunDef Core'
      typeCheckFun undefined bad_fun `shouldSatisfy` isLeft
    it "assign" $ do
      let prog =
            Program
              [ NamedFunDef "main" $
                  FunDef
                    { param_types = [Fin 2]
                    , mbody =
                        Just
                          FunBody
                            { param_names = ["x"]
                            , body_stmt = ExprS{rets = ["y"], expr = BasicExprE $ VarE "x"}
                            , ret_names = ["y"]
                            }
                    , ret_types = [Fin 2]
                    }
              ] ::
              Program Core'
      assertRight $ typeCheckProg prog
    it "matrix example" $ do
      assertRight $ typeCheckProg (matrixExampleS 4 5)

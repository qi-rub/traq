module Traq.CPL.TypeCheckSpec (spec) where

import Traq.CPL.Syntax
import Traq.CPL.TypeCheck
import Traq.Examples.MatrixSearch (matrixExampleS)

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typecheck" $ do
    describe "assign" $ do
      it "y=x" $ do
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
      it "x=x" $ do
        let prog =
              Program
                [ NamedFunDef "main" $
                    FunDef
                      { param_types = [Fin 2]
                      , mbody =
                          Just
                            FunBody
                              { param_names = ["x"]
                              , body_stmt = ExprS{rets = ["x"], expr = BasicExprE $ VarE "x"}
                              , ret_names = ["x"]
                              }
                      , ret_types = [Fin 2]
                      }
                ] ::
                Program Core'
        assertRight $ typeCheckProg prog
    it "matrix example" $ do
      assertRight $ typeCheckProg (matrixExampleS 4 5)

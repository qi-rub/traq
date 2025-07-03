module Traq.ProtoLang.TypeCheckSpec (spec) where

import Data.Either (isLeft)
import Data.Void (Void)

import qualified Traq.Data.Context as Ctx

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
              FunDef Void Int
      typeCheckFun undefined bad_fun `shouldSatisfy` isLeft
    it "assign" $ do
      let prog =
            Program
              { funCtx = Ctx.empty
              , stmt = ExprS{rets = ["y"], expr = BasicExprE $ VarE "x"}
              } ::
              Program Void Int
      let gamma = Ctx.fromList [("x", Fin 2)]
      assertRight $ typeCheckProg gamma prog
    it "matrix example" $ do
      assertRight $ typeCheckProg Ctx.empty (matrixExampleS 4 5)

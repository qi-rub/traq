module QCompose.ProtoLang.TypeCheckSpec (spec) where

import Data.Either (isLeft)

import qualified QCompose.Data.Context as Ctx

import QCompose.Examples.MatrixSearch (matrixExampleS)
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.TypeCheck

import Test.Hspec
import TestHelpers

emptyFunCtx :: FunCtx a
emptyFunCtx = FunCtx{oracle_decl = undefined, fun_defs = Ctx.empty}

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
    it "assign" $ do
      let prog =
            Program
              { funCtx = emptyFunCtx
              , stmt = ExprS{rets = ["y"], expr = VarE "x"}
              } ::
              Program Int
      let gamma = Ctx.fromList [("x", Fin 2)]
      assertRight $ typeCheckProg gamma prog
    it "matrix example" $ do
      assertRight $ typeCheckProg Ctx.empty (matrixExampleS 4 5)

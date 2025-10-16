{-# LANGUAGE OverloadedStrings #-}

module Traq.Compiler.UnitarySpec (spec) where

import Data.Void (Void)

import qualified Traq.Data.Context as Ctx

import qualified Traq.CQPL as CQPL
import Traq.Compiler.Unitary
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $
      do
        let main_fun =
              P.FunDef
                { P.param_types = [P.Fin 10]
                , P.mbody =
                    Just
                      P.FunBody
                        { P.param_names = ["x"]
                        , P.body_stmt = P.ExprS{P.rets = ["y"], P.expr = P.BasicExprE P.VarE{P.var = "x"}} :: P.Stmt Void Int
                        , P.ret_names = ["y"]
                        }
                , P.ret_types = [P.Fin 10]
                }

        actual <-
          expectRight $
            lowerProgram
              P.SplitSimple
              (Ctx.singleton "x" (P.Fin 10))
              (P.l2NormError (0 :: Double))
              (P.Program [P.NamedFunDef "main" main_fun])

        assertRight $ CQPL.typeCheckProgram actual
        PP.toCodeString actual
          `shouldBe` unlines
            [ "// main[0.0]"
            , "uproc main(x : IN Fin<10>, y : OUT Fin<10>) {"
            , "  x, y *= Embed[(x) => x];"
            , "}"
            , ""
            ]

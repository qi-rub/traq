{-# LANGUAGE OverloadedStrings #-}

module Traq.Compiler.UnitarySpec (spec) where

import qualified Data.Map as Map
import Data.Void (Void)
import qualified Traq.Data.Context as Ctx

import Traq.Compiler.Unitary
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as U
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $
      do
        let s = P.ExprS{P.rets = ["y"], P.expr = P.BasicExprE P.VarE{P.var = "x"}} :: P.Stmt Void Int

        actual <-
          expectRight $
            lowerProgram
              P.SplitSimple
              (Ctx.singleton "x" (P.Fin 10))
              (Map.singleton "Oracle" 1.0)
              (0 :: Double)
              P.Program
                { P.funCtx = Ctx.empty
                , P.stmt = s
                }

        let expected =
              U.Program
                { U.proc_defs =
                    Ctx.singleton
                      "main"
                      U.UProcDef
                        { U.info_comment = ""
                        , U.proc_name = "main"
                        , U.proc_meta_params = []
                        , U.proc_params = [("x", U.ParamUnk, P.Fin 10), ("y", U.ParamUnk, P.Fin 10)]
                        , U.proc_body_or_tick = Right (U.UnitaryS ["x", "y"] $ U.RevEmbedU ["x"] "x")
                        }
                } ::
                U.Program Void Int Double

        actual `shouldBe` expected
        assertRight $ U.typeCheckProgram actual
        PP.toCodeString actual
          `shouldBe` unlines
            [ "uproc main(x : Fin<10>, y : Fin<10>) {"
            , "  x, y *= Embed[(x) => x];"
            , "}"
            , ""
            ]

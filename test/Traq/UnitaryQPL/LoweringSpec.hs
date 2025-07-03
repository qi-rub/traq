{-# LANGUAGE OverloadedStrings #-}

module Traq.UnitaryQPL.LoweringSpec (spec) where

import qualified Data.Map as Map
import Data.Void (Void)
import qualified Traq.Data.Context as Ctx

import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as U
import Traq.Utils.Printing

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $
      do
        let s = P.ExprS{P.rets = ["y"], P.expr = P.BasicExprE P.VarE{P.var = "x"}} :: P.Stmt Void Int

        (actual, gamma) <-
          expectRight $
            U.lowerProgram
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
                { U.proc_defs = Ctx.empty
                , U.stmt = U.UnitaryS ["x", "y"] $ U.RevEmbedU ["x"] "x"
                } ::
                U.Program Void Int Double

        actual `shouldBe` expected

        toCodeString actual
          `shouldBe` "x, y *= Embed[(x) => x];\n\n"

        gamma `shouldBe` Ctx.fromList [("x", P.Fin 10), ("y", P.Fin 10)]

        assertRight $ U.typeCheckProgram gamma actual

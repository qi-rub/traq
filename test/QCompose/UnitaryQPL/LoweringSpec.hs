module QCompose.UnitaryQPL.LoweringSpec (spec) where

import Data.Void (Void)
import qualified QCompose.Data.Context as Ctx

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U
import QCompose.Utils.Printing

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $
      do
        let s = P.ExprS{P.rets = ["y"], P.expr = P.VarE{P.arg = "x"}} :: P.Stmt Void Int

        (actual, gamma) <-
          expectRight $
            U.lowerProgram
              (Ctx.singleton "x" (P.Fin 10))
              "Oracle"
              (0 :: Double)
              P.Program
                { P.funCtx = Ctx.empty
                , P.stmt = s
                }

        let expected =
              U.Program
                { U.proc_defs = Ctx.empty
                , U.stmt = U.UnitaryS ["x", "y"] $ U.RevEmbedU (U.IdF (P.Fin 10))
                } ::
                U.Program Void Int Double

        actual `shouldBe` expected

        toCodeString actual
          `shouldBe` "x, y *= RevEmbed[x : Fin<10> => x];\n\n"

        gamma `shouldBe` Ctx.fromList [("x", P.Fin 10), ("y", P.Fin 10)]

        assertRight $ U.typeCheckProgram gamma actual

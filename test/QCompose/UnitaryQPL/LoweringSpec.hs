module QCompose.UnitaryQPL.LoweringSpec (spec) where

import Lens.Micro
import qualified QCompose.Data.Context as Ctx

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

import QCompose.Primitives.QSearch
import QCompose.Utils.Printing

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $
      do
        let s = P.ExprS{P.rets = ["y"], P.expr = P.VarE{P.arg = "x"}} :: P.Stmt Int
        (res, gamma) <-
          expectRight $
            U.lowerProgram
              (qsearchCFNW ^. to unitaryAlgo)
              (Ctx.singleton "x" (P.Fin 10))
              "Oracle"
              (0 :: Double)
              P.Program
                { P.funCtx = Ctx.empty
                , P.stmt = s
                }
        res
          `shouldBe` U.Program
            { U.proc_defs = Ctx.empty
            , U.stmt = U.UnitaryS ["x", "y"] $ U.RevEmbedU (U.IdF (P.Fin 10))
            }

        toCodeString res
          `shouldBe` unlines
            [ "uproc Oracle(Fin<2>)\n"
            , "x, y *= RevEmbed[x : Fin<10> => x];"
            , ""
            ]

        gamma `shouldBe` Ctx.fromList [("x", P.Fin 10), ("y", P.Fin 10)]

        assertRight $ U.typeCheckProgram gamma res

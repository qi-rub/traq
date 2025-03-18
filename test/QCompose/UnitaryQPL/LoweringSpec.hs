module QCompose.UnitaryQPL.LoweringSpec (spec) where

import qualified Data.Map as M

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as U

import QCompose.Subroutines.QSearch
import QCompose.Utils.Printing

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    let dummy_oracle = P.OracleDecl{P.param_types = [], P.ret_types = [P.Fin 2]}
    it "assign" $
      do
        let s = P.ExprS{P.rets = ["y"], P.expr = P.VarE{P.arg = "x"}} :: P.Stmt Int
        (res, gamma) <-
          expectRight $
            U.lowerProgram
              zalkaQSearch
              (M.singleton "x" (P.Fin 10))
              0
              P.Program
                { P.funCtx = P.FunCtx{P.oracle_decl = dummy_oracle, P.fun_defs = []}
                , P.stmt = s
                }
        res
          `shouldBe` U.Program
            { U.oracle_decl = dummy_oracle
            , U.proc_defs = []
            , U.stmt = U.UnitaryS ["x", "y"] $ U.RevEmbedU (U.IdF (P.Fin 10))
            }

        toCodeString res
          `shouldBe` unlines
            [ "declare Oracle() -> Fin<2>\n"
            , "x, y *= RevEmbed[x : Fin<10> => x];"
            , ""
            ]

        gamma `shouldBe` M.fromList [("x", P.Fin 10), ("y", P.Fin 10)]

        assertRight $ U.typeCheckProgram gamma res

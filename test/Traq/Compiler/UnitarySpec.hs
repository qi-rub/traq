{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Compiler.UnitarySpec (spec) where

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CPL as CPL
import Traq.Compiler.Unitary
import Traq.Prelude
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

type SymCore = CPL.Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "lowerStmt" $ do
    it "assign" $ do
      let main_fun =
            CPL.FunDef
              { CPL.param_types = [CPL.Fin 10]
              , CPL.mbody =
                  Just
                    CPL.FunBody
                      { CPL.param_names = ["x"]
                      , CPL.body_stmt = CPL.ExprS{CPL.rets = ["y"], CPL.expr = CPL.BasicExprE CPL.VarE{CPL.var = "x"}} :: CPL.Stmt CPL.Core'
                      , CPL.ret_names = ["y"]
                      }
              , CPL.ret_types = [CPL.Fin 10]
              }

      actual <-
        expectRight $
          lowerProgramU (CPL.Program [CPL.NamedFunDef "main" main_fun])

      assertRight $ QPL.typeCheckProgram actual
      PP.toCodeString actual
        `shouldBe` unlines
          [ "uproc main_U(x : IN Fin<10>, y : OUT Fin<10>, y_1 : AUX Fin<10>) {"
          , "  x, y_1 *= Embed[(x) => x];"
          , "  y, y_1 *= SWAP;"
          , "}"
          , ""
          ]

    it "ifte" $ do
      ex <-
        expectRight $
          CPL.parseProgram @SymCore
            "fn ternary(b: Bool, x: Fin<10>, y: Fin<10>) -> Fin<10> do if (b) then r <- x; else r <- y; end return r end"
      let ex' = CPL.mapSize Sym.unSym ex
      assertRight $ lowerProgramU ex'

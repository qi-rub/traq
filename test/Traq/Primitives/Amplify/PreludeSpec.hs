{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Amplify.PreludeSpec (spec) where

import qualified Data.Map as Map
import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CPL as CPL
import Traq.Primitives.Amplify.Prelude (Amplify (..))
import Traq.Primitives.Class
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

exampleProgram1 :: (Num size, Fractional prec) => size -> CPL.Program (Primitive (Amplify size prec))
exampleProgram1 n = CPL.Program [CPL.NamedFunDef "sampler" sampler, CPL.NamedFunDef "main" main_fun]
 where
  node_ty = CPL.Fin n

  sampler =
    CPL.FunDef
      { CPL.param_types = [node_ty]
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody = Nothing
      }

  main_fun =
    CPL.FunDef
      { CPL.param_types = []
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody =
          Just $
            CPL.FunBody
              { CPL.param_names = []
              , CPL.ret_names = ["ok", "result"]
              , CPL.body_stmt = CPL.SeqS [stmt_x, amplify_call]
              }
      }

  stmt_x =
    CPL.ExprS
      { CPL.rets = ["x"]
      , CPL.expr =
          CPL.BasicExprE
            CPL.ConstE
              { CPL.val = CPL.FinV 1
              , CPL.ty = node_ty
              }
      }

  amplify_call =
    CPL.ExprS
      { CPL.rets = ["ok", "result"]
      , CPL.expr =
          CPL.PrimCallE $
            Primitive [PartialFun{pfun_name = "sampler", pfun_args = [Just "x"]}] $
              Amplify{p_min = 0.02}
      }

exampleProgram2 :: (Num size, Fractional prec) => size -> CPL.Program (Primitive (Amplify size prec))
exampleProgram2 n = CPL.Program [CPL.NamedFunDef "f" fDef, CPL.NamedFunDef "main" mainDef]
 where
  node_ty = CPL.Fin n

  funBody =
    CPL.FunBody
      { CPL.param_names = ["y"]
      , CPL.ret_names = ["ok", "x"]
      , CPL.body_stmt =
          CPL.SeqS
            [ CPL.ExprS
                { CPL.rets = ["x"]
                , CPL.expr =
                    CPL.RandomSampleE
                      { CPL.distr_expr = CPL.UniformE{CPL.sample_ty = node_ty}
                      }
                }
            , CPL.ExprS
                { CPL.rets = ["ok"]
                , CPL.expr =
                    CPL.BasicExprE $
                      CPL.BinOpE
                        { CPL.bin_op = CPL.LEqOp
                        , CPL.lhs = CPL.VarE "y"
                        , CPL.rhs = CPL.VarE "x"
                        }
                }
            ]
      }

  fDef =
    CPL.FunDef
      { CPL.param_types = [node_ty]
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody = Just funBody
      }

  stmt_y =
    CPL.ExprS
      { CPL.rets = ["y"]
      , CPL.expr =
          CPL.BasicExprE $
            CPL.ConstE
              { CPL.val = CPL.FinV 1
              , CPL.ty = node_ty
              }
      }

  amplify_call =
    CPL.ExprS
      { CPL.rets = ["ok", "result"]
      , CPL.expr =
          CPL.PrimCallE $
            Primitive [PartialFun{pfun_name = "f", pfun_args = [Just "y"]}] $
              Amplify{p_min = 0.6}
      }

  mainDef =
    CPL.FunDef
      { CPL.param_types = []
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody =
          Just $
            CPL.FunBody
              { CPL.param_names = []
              , CPL.ret_names = ["ok", "result"]
              , CPL.body_stmt = CPL.SeqS [stmt_y, amplify_call]
              }
      }

exampleProgram3 :: (Num size, Fractional prec) => size -> CPL.Program (Primitive (Amplify size prec))
exampleProgram3 n = CPL.Program [CPL.NamedFunDef "sampler" sampler, CPL.NamedFunDef "main" mainDef]
 where
  node_ty = CPL.Fin n

  funBody =
    CPL.FunBody
      { CPL.param_names = ["l", "r"]
      , CPL.ret_names = ["ok", "x"]
      , CPL.body_stmt =
          CPL.SeqS
            [ CPL.ExprS
                { CPL.rets = ["x"]
                , CPL.expr = CPL.RandomSampleE $ CPL.UniformE{CPL.sample_ty = node_ty}
                }
            , CPL.ExprS
                { CPL.rets = ["ok_l"]
                , CPL.expr =
                    CPL.BasicExprE $
                      CPL.BinOpE
                        { CPL.bin_op = CPL.LEqOp
                        , CPL.lhs = CPL.VarE "l"
                        , CPL.rhs = CPL.VarE "x"
                        }
                }
            , CPL.ExprS
                { CPL.rets = ["ok_r"]
                , CPL.expr =
                    CPL.BasicExprE $
                      CPL.BinOpE
                        { CPL.bin_op = CPL.LEqOp
                        , CPL.lhs = CPL.VarE "x"
                        , CPL.rhs = CPL.VarE "r"
                        }
                }
            , CPL.ExprS
                { CPL.rets = ["ok"]
                , CPL.expr =
                    CPL.BasicExprE $
                      CPL.BinOpE
                        { CPL.bin_op = CPL.AndOp
                        , CPL.lhs = CPL.VarE "ok_l"
                        , CPL.rhs = CPL.VarE "ok_r"
                        }
                }
            ]
      }

  sampler =
    CPL.FunDef
      { CPL.param_types = [node_ty, node_ty]
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody = Just funBody
      }

  stmt_l =
    CPL.ExprS
      { CPL.rets = ["l"]
      , CPL.expr =
          CPL.BasicExprE $
            CPL.ConstE
              { CPL.val = CPL.FinV 1
              , CPL.ty = node_ty
              }
      }

  stmt_r =
    CPL.ExprS
      { CPL.rets = ["r"]
      , CPL.expr =
          CPL.BasicExprE $
            CPL.ConstE
              { CPL.val = CPL.FinV 4
              , CPL.ty = node_ty
              }
      }

  amplify_call =
    CPL.ExprS
      { CPL.rets = ["ok", "x"]
      , CPL.expr =
          CPL.PrimCallE $
            Primitive [PartialFun{pfun_name = "sampler", pfun_args = [Just "l", Just "r"]}] $
              Amplify{p_min = 0.2}
      }

  mainDef =
    CPL.FunDef
      { CPL.param_types = []
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody =
          Just $
            CPL.FunBody
              { CPL.param_names = []
              , CPL.ret_names = ["ok", "x"]
              , CPL.body_stmt = CPL.SeqS [stmt_l, stmt_r, amplify_call]
              }
      }

spec :: Spec
spec = describe "amplify" $ do
  describe "amplify example1" $ do
    it "parses" $ do
      p <-
        parseFromFile
          CPL.programParser
          "examples/primitives/amplify/amplify1.traq"
          >>= expectRight
      p `shouldBe` exampleProgram1 @(Sym.Sym Int) @Double (Sym.var "N")

    it "round-trips through print/parse" $ do
      let stmt = "ok, result <- @amplify<2.0e-2>[f(x1, x2)];\n"
      let parsed = CPL.parseStmt @(Primitive (Amplify (Sym.Sym Int) Double)) stmt
      let printed = PP.toCodeString <$> parsed
      printed `shouldBe` Right stmt

    let program1 = exampleProgram1 20

    it "type checks" $ do
      assertRight $ CPL.typeCheckProg program1

    it "evaluates" $ do
      let funInterpCtx = Map.singleton "sampler" (\inp -> CPL.toValue True : inp)
      let result = CPL.runProgram program1 funInterpCtx []

      result `shouldBeDistribution` [([CPL.FinV 1, CPL.FinV 1], 1 :: Double)]

  describe "amplify example2" $ do
    it "parses" $ do
      p <-
        parseFromFile
          CPL.programParser
          "examples/primitives/amplify/amplify2.traq"
          >>= expectRight
      p `shouldBe` exampleProgram2 @(Sym.Sym Int) @Double (Sym.var "N")

    let program2 = exampleProgram2 3

    it "type checks" $ do
      assertRight $ CPL.typeCheckProg program2

    it "evaluates" $ do
      let funInterpCtx = Map.singleton "sampler" (\case [CPL.FinV x1, CPL.FinV _] -> [CPL.toValue True, CPL.FinV x1]; _ -> undefined)
      let result = CPL.runProgram program2 funInterpCtx []

      result
        `shouldBeDistribution` [ ([CPL.FinV 1, CPL.FinV 1], 0.5 :: Double)
                               , ([CPL.FinV 1, CPL.FinV 2], 0.5)
                               ]

  describe "amplify example3" $ do
    it "parses" $ do
      p <-
        parseFromFile
          CPL.programParser
          "examples/primitives/amplify/amplify3.traq"
          >>= expectRight
      p `shouldBe` exampleProgram3 @(Sym.Sym Int) @Double (Sym.var "N")

    let program3 = exampleProgram3 3

    it "type checks" $ do
      assertRight $ CPL.typeCheckProg program3

    it "evaluates" $ do
      let funInterpCtx = Map.singleton "sampler" (\case [CPL.FinV x1, CPL.FinV _] -> [CPL.toValue True, CPL.FinV x1]; _ -> undefined)

      let result = CPL.runProgram program3 funInterpCtx []

      result
        `shouldBeDistribution` [ ([CPL.FinV 1, CPL.FinV 1], 0.5 :: Double)
                               , ([CPL.FinV 1, CPL.FinV 2], 0.5)
                               ]

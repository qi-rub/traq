{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.AmplifySpec (spec) where

import Text.Parsec.String

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Primitives.Amplify.Prelude (Amplify (..))
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

exampleProgram1 :: (Num sizeT) => sizeT -> P.Program Amplify sizeT
exampleProgram1 n = P.Program [P.NamedFunDef "sampler" sampler, P.NamedFunDef "main" main_fun]
 where
  node_ty = P.Fin n

  sampler =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody = Nothing
      }

  main_fun =
    P.FunDef
      { P.param_types = []
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody =
          Just $
            P.FunBody
              { P.param_names = []
              , P.ret_names = ["ok", "result"]
              , P.body_stmt = P.SeqS [stmt_x, amplify_call]
              }
      }

  stmt_x =
    P.ExprS
      { P.rets = ["x"]
      , P.expr =
          P.BasicExprE
            P.ConstE
              { P.val = P.FinV 1
              , P.ty = node_ty
              }
      }

  amplify_call =
    P.ExprS
      { P.rets = ["ok", "result"]
      , P.expr =
          P.PrimCallE
            Amplify{sampler = "sampler", p_min = 0.02, sampler_args = ["x"]}
      }

exampleProgram2 :: (Num sizeT) => sizeT -> P.Program Amplify sizeT
exampleProgram2 n = P.Program [P.NamedFunDef "f" fDef, P.NamedFunDef "main" mainDef]
 where
  node_ty = P.Fin n

  funBody =
    P.FunBody
      { P.param_names = ["y"]
      , P.ret_names = ["ok", "x"]
      , P.body_stmt =
          P.SeqS
            [ P.ExprS
                { P.rets = ["x"]
                , P.expr =
                    P.RandomSampleE
                      { P.distr_expr = P.UniformE{P.sample_ty = node_ty}
                      }
                }
            , P.ExprS
                { P.rets = ["ok"]
                , P.expr =
                    P.BasicExprE $
                      P.BinOpE
                        { P.bin_op = P.LEqOp
                        , P.lhs = P.VarE "y"
                        , P.rhs = P.VarE "x"
                        }
                }
            ]
      }

  fDef =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody = Just funBody
      }

  stmt_y =
    P.ExprS
      { P.rets = ["y"]
      , P.expr =
          P.BasicExprE $
            P.ConstE
              { P.val = P.FinV 1
              , P.ty = node_ty
              }
      }

  amplify_call =
    P.ExprS
      { P.rets = ["ok", "result"]
      , P.expr =
          P.PrimCallE
            Amplify{sampler = "f", p_min = 0.6, sampler_args = ["y"]}
      }

  mainDef =
    P.FunDef
      { P.param_types = []
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody =
          Just $
            P.FunBody
              { P.param_names = []
              , P.ret_names = ["ok", "result"]
              , P.body_stmt = P.SeqS [stmt_y, amplify_call]
              }
      }

exampleProgram3 :: (Num sizeT) => sizeT -> P.Program Amplify sizeT
exampleProgram3 n = P.Program [P.NamedFunDef "sampler" sampler, P.NamedFunDef "main" mainDef]
 where
  node_ty = P.Fin n

  funBody =
    P.FunBody
      { P.param_names = ["l", "r"]
      , P.ret_names = ["ok", "x"]
      , P.body_stmt =
          P.SeqS
            [ P.ExprS
                { P.rets = ["x"]
                , P.expr = P.RandomSampleE $ P.UniformE{P.sample_ty = node_ty}
                }
            , P.ExprS
                { P.rets = ["ok_l"]
                , P.expr =
                    P.BasicExprE $
                      P.BinOpE
                        { P.bin_op = P.LEqOp
                        , P.lhs = P.VarE "l"
                        , P.rhs = P.VarE "x"
                        }
                }
            , P.ExprS
                { P.rets = ["ok_r"]
                , P.expr =
                    P.BasicExprE $
                      P.BinOpE
                        { P.bin_op = P.LEqOp
                        , P.lhs = P.VarE "x"
                        , P.rhs = P.VarE "r"
                        }
                }
            , P.ExprS
                { P.rets = ["ok"]
                , P.expr =
                    P.BasicExprE $
                      P.BinOpE
                        { P.bin_op = P.AndOp
                        , P.lhs = P.VarE "ok_l"
                        , P.rhs = P.VarE "ok_r"
                        }
                }
            ]
      }

  sampler =
    P.FunDef
      { P.param_types = [node_ty, node_ty]
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody = Just funBody
      }

  stmt_l =
    P.ExprS
      { P.rets = ["l"]
      , P.expr =
          P.BasicExprE $
            P.ConstE
              { P.val = P.FinV 1
              , P.ty = node_ty
              }
      }

  stmt_r =
    P.ExprS
      { P.rets = ["r"]
      , P.expr =
          P.BasicExprE $
            P.ConstE
              { P.val = P.FinV 4
              , P.ty = node_ty
              }
      }

  amplify_call =
    P.ExprS
      { P.rets = ["ok", "x"]
      , P.expr =
          P.PrimCallE
            Amplify{sampler = "sampler", p_min = 0.2, sampler_args = ["l", "r"]}
      }

  mainDef =
    P.FunDef
      { P.param_types = []
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody =
          Just $
            P.FunBody
              { P.param_names = []
              , P.ret_names = ["ok", "x"]
              , P.body_stmt = P.SeqS [stmt_l, stmt_r, amplify_call]
              }
      }

spec :: Spec
spec = describe "amplify" $ do
  describe "amplify example1" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify1.qb"
          >>= expectRight
      p `shouldBe` exampleProgram1 (Sym.var "N")

    it "round-trips through print/parse" $ do
      let stmt = "ok, result <- @amplify[f, 0.02](x1, x2);\n"
      let parsed = P.parseStmt @Amplify stmt
      let printed = PP.toCodeString <$> parsed
      printed `shouldBe` Right stmt

    let program1 = exampleProgram1 20

    it "type checks" $ do
      assertRight $ P.typeCheckProg program1

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1] -> [P.toValue True, P.FinV x1])
      let result = P.runProgram program1 funInterpCtx []

      result `shouldBeDistribution` [([P.FinV 1, P.FinV 1], 1 :: Double)]

  describe "amplify example2" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify2.qb"
          >>= expectRight
      p `shouldBe` exampleProgram2 (Sym.var "N")

    let program2 = exampleProgram2 3

    it "type checks" $ do
      assertRight $ P.typeCheckProg program2

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1, P.FinV _] -> [P.toValue True, P.FinV x1])
      let result = P.runProgram program2 funInterpCtx []

      result
        `shouldBeDistribution` [ ([P.FinV 1, P.FinV 1], 0.5 :: Double)
                               , ([P.FinV 1, P.FinV 2], 0.5)
                               ]

  describe "amplify example3" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify3.qb"
          >>= expectRight
      p `shouldBe` exampleProgram3 (Sym.var "N")

    let program3 = exampleProgram3 3

    it "type checks" $ do
      assertRight $ P.typeCheckProg program3

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1, P.FinV _] -> [P.toValue True, P.FinV x1])

      let result = P.runProgram program3 funInterpCtx []

      result
        `shouldBeDistribution` [ ([P.FinV 1, P.FinV 1], 0.5 :: Double)
                               , ([P.FinV 1, P.FinV 2], 0.5)
                               ]

{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.AmplifySpec (spec) where

import Text.Parsec.String

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Traq.Primitives.Amplify (QAmplify (..))

import Test.Hspec
import TestHelpers

exampleProgram1 :: (Num sizeT) => sizeT -> sizeT -> P.Program QAmplify sizeT
exampleProgram1 two n = P.Program{P.funCtx = fun_ctx, P.stmt = P.SeqS [stmt_x, amplify_call]}
 where
  bool_ty = P.Fin two
  node_ty = P.Fin n

  -- sampler of type (Fin<N>) -> (Bool, Fin<N>)
  sampler =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [bool_ty, node_ty]
      , P.mbody = Nothing
      }

  fun_ctx = Ctx.fromList [("sampler", sampler)]

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
          P.FunCallE
            { P.fun_kind = P.PrimitiveCall QAmplify{sampler = "sampler", p_min = 0.02}
            , P.args = ["x"]
            }
      }

exampleProgram2 :: (Num sizeT) => sizeT -> sizeT -> P.Program QAmplify sizeT
exampleProgram2 two n =
  P.Program
    { P.funCtx = fun_ctx
    , P.stmt = P.SeqS [stmt_y, amplify_call]
    }
 where
  bool_ty = P.Fin two
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
                    P.UniformRandomE
                      { P.sample_ty = node_ty
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
      , P.ret_types = [bool_ty, node_ty]
      , P.mbody = Just funBody
      }

  fun_ctx = Ctx.fromList [("f", fDef)]

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
          P.FunCallE
            { P.fun_kind = P.PrimitiveCall QAmplify{sampler = "f", p_min = 0.6}
            , P.args = ["y"]
            }
      }

exampleProgram3 :: (Num sizeT) => sizeT -> sizeT -> P.Program QAmplify sizeT
exampleProgram3 two n =
  P.Program
    { P.funCtx = fun_ctx
    , P.stmt = P.SeqS [stmt_l, stmt_r, amplify_call]
    }
 where
  bool_ty = P.Fin two
  node_ty = P.Fin n

  funBody =
    P.FunBody
      { P.param_names = ["l", "r"]
      , P.ret_names = ["ok", "x"]
      , P.body_stmt =
          P.SeqS
            [ P.ExprS
                { P.rets = ["x"]
                , P.expr = P.UniformRandomE{P.sample_ty = node_ty}
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
      , P.ret_types = [bool_ty, node_ty]
      , P.mbody = Just funBody
      }

  fun_ctx = Ctx.fromList [("sampler", sampler)]

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
          P.FunCallE
            { P.fun_kind = P.PrimitiveCall QAmplify{sampler = "sampler", p_min = 0.2}
            , P.args = ["l", "r"]
            }
      }

spec :: Spec
spec = do
  describe "amplify example1" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify1.qb"
          >>= expectRight
      p `shouldBe` exampleProgram1 (Sym.con 2) (Sym.var "N")

    it "round-trips through print/parse" $ do
      let stmt = "ok, result <- @amplify[f, 0.02]\n(x1, x2);\n"
      let parsed = P.parseStmt @QAmplify stmt
      let printed = PP.toCodeString <$> parsed
      printed `shouldBe` Right stmt

    let program1 = exampleProgram1 2 20

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty program1

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1] -> [P.toValue True, P.FinV x1])
      let initialState = Ctx.empty
      let result = P.runProgram program1 funInterpCtx initialState

      result `shouldBeDistribution` [(Ctx.fromList [("x", P.FinV 1), ("ok", P.FinV 1), ("result", P.FinV 1)], 1 :: Double)]

  describe "amplify example2" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify2.qb"
          >>= expectRight
      p `shouldBe` exampleProgram2 (Sym.con 2) (Sym.var "N")

    let program2 = exampleProgram2 2 3

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty program2

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1, P.FinV x2] -> [P.toValue True, P.FinV x1])
      let initialState = Ctx.empty
      let result = P.runProgram program2 funInterpCtx initialState

      result
        `shouldBeDistribution` [ (Ctx.fromList [("y", P.FinV 1), ("ok", P.FinV 1), ("result", P.FinV 1)], 0.5 :: Double)
                               , (Ctx.fromList [("y", P.FinV 1), ("ok", P.FinV 1), ("result", P.FinV 2)], 0.5)
                               ]

  describe "amplify example3" $ do
    it "parses" $ do
      p <-
        parseFromFile
          P.programParser
          "examples/primitives/amplify/amplify3.qb"
          >>= expectRight
      p `shouldBe` exampleProgram3 (Sym.con 2) (Sym.var "N")

    let program3 = exampleProgram3 2 3

    it "type checks" $ do
      assertRight $ P.typeCheckProg Ctx.empty program3

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "sampler" (\[P.FinV x1, P.FinV x2] -> [P.toValue True, P.FinV x1])
      let initialState = Ctx.empty

      let result = P.runProgram program3 funInterpCtx initialState

      result
        `shouldBeDistribution` [ (Ctx.fromList [("l", P.FinV 1), ("r", P.FinV 4), ("ok", P.FinV 1), ("x", P.FinV 1)], 0.5 :: Double)
                               , (Ctx.fromList [("l", P.FinV 1), ("r", P.FinV 4), ("ok", P.FinV 1), ("x", P.FinV 2)], 0.5)
                               ]

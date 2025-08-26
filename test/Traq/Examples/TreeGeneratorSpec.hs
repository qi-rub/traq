{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGeneratorSpec where

import Text.Parsec.String

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import qualified Traq.Data.Symbolic as Sym

import Traq.Examples.TreeGenerator (treeGeneratorExample)
import Traq.Primitives
import Traq.ProtoLang
import Traq.ProtoLang.Syntax (Value (ArrV, FinV))
import Traq.ProtoLang.TypeCheck (typeCheckProg)

import Test.Hspec
import TestHelpers

loopExample :: (Num sizeT) => sizeT -> sizeT -> Program primT sizeT
loopExample n w =
  Program
    [ NamedFunDef
        { fun_name = "AddWeight"
        , fun_def =
            FunDef
              { param_types = [Fin w, Fin n]
              , ret_types = [Fin w]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["acc", "i"]
                      , ret_names = ["acc'"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["one"] $ BasicExprE $ ConstE (FinV 1) (Fin w)
                            , ExprS ["acc'"] $
                                BasicExprE $
                                  BinOpE AddOp (VarE "acc") (VarE "one")
                            ]
                      }
              }
        }
    , NamedFunDef
        { fun_name = "main"
        , fun_def =
            FunDef
              { param_types = []
              , ret_types = [Fin w]
              , mbody =
                  Just
                    FunBody
                      { param_names = []
                      , ret_names = ["tw"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["acc"] $ BasicExprE $ ConstE (FinV 0) (Fin w)
                            , ExprS ["tw"] $
                                LoopE
                                  { initial_args = [VarE "acc"]
                                  , loop_body_fun = "AddWeight"
                                  }
                            ]
                      }
              }
        }
    ]

spec :: Spec
spec = do
  describe "Tree Generator Example" $ do
    it "parses" $ do
      p <-
        parseFromFile (programParser @DefaultPrims) "examples/tree_generator/tree_generator.qb"
          >>= expectRight
      p `shouldBe` treeGeneratorExample (Sym.var "N") (Sym.var "W") (Sym.var "P")

    it "typechecks" $ do
      p <-
        parseFromFile (programParser @DefaultPrims) "examples/tree_generator/tree_generator.qb"
          >>= expectRight
      assertRight $ (typeCheckProg @DefaultPrims) p

    it "evaluates" $ do
      let n = 1
      let funInterpCtx =
            Ctx.fromList
              [ ("Capacity", \_ -> [FinV 20])
              , ("Profit", \[FinV i] -> [FinV i])
              , ("Weight", \[FinV i] -> [FinV i])
              ]
      let result = (runProgram @DefaultPrims) (treeGeneratorExample n 20 30) funInterpCtx [ArrV (replicate n (FinV 0))]

      result
        `shouldBeDistribution` [([FinV 0, ArrV [FinV 0]], 0.5), ([FinV 0, ArrV [FinV 1]], 0.5)]

  describe "Loop example" $ do
    it "parses" $ do
      p <-
        parseFromFile (programParser @DefaultPrims) "examples/tree_generator/loop_example.qb"
          >>= expectRight
      p `shouldBe` loopExample (Sym.var "N") (Sym.var "W")

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "AddWeight" (\[FinV x1, FinV x2] -> [FinV x1])
      let result = (runProgram @DefaultPrims) (loopExample 10 20) funInterpCtx []

      result
        `shouldBeDistribution` [([FinV 10], 1.0)]

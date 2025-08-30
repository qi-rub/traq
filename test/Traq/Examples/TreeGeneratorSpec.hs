{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGeneratorSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob
import qualified Traq.Data.Symbolic as Sym

import Traq.Examples.TreeGenerator (treeGeneratorExample)
import Traq.Prelude
import Traq.Primitives.Amplify.Prelude (Amplify)
import Traq.ProtoLang

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

loadKnapsack ::
  -- | number of elements
  SizeT ->
  -- | bound on total weight
  SizeT ->
  -- | bound on total profit
  SizeT ->
  -- | number of iterations
  SizeT ->
  IO (Program Amplify SizeT)
loadKnapsack n w p k = do
  Right prog <- parseFromFile (programParser @Amplify) "examples/tree_generator/tree_generator_01_knapsack.qb"
  return $
    prog
      <&> Sym.subst "N" (Sym.con n)
      <&> Sym.subst "W" (Sym.con w)
      <&> Sym.subst "P" (Sym.con p)
      <&> Sym.subst "K" (Sym.con k)
      <&> Sym.unSym

spec :: Spec
spec = do
  describe "Tree Generator Example" $ do
    it "parses" $ do
      expectRight =<< parseFromFile (programParser @Amplify) "examples/tree_generator/tree_generator_01_knapsack.qb"
      -- p `shouldBe` treeGeneratorExample (Sym.var "N") (Sym.var "W") (Sym.var "P")
      return ()

    it "typechecks" $ do
      p <-
        parseFromFile (programParser @Amplify) "examples/tree_generator/tree_generator_01_knapsack.qb"
          >>= expectRight
      assertRight $ (typeCheckProg @Amplify) p

    it "evaluates" $ do
      let n = 2
      prog <- loadKnapsack n 20 30 2
      let funInterpCtx =
            Ctx.fromList
              [ ("Capacity", \_ -> [FinV 20])
              , ("Profit", \[FinV i] -> [FinV i])
              , ("Weight", \[FinV i] -> [FinV i])
              ]
      let result = (runProgram @Amplify) prog funInterpCtx []

      result
        `shouldBeDistribution` [ ([ArrV [FinV 0, FinV 1]], 0.8)
                               , ([ArrV [FinV 1, FinV 1]], 0.2)
                               ]

  describe "Loop example" $ do
    it "parses" $ do
      p <-
        parseFromFile (programParser @Amplify) "examples/tree_generator/loop_example.qb"
          >>= expectRight
      p `shouldBe` loopExample (Sym.var "N") (Sym.var "W")

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "AddWeight" (\[FinV x1, FinV x2] -> [FinV x1])
      let result = (runProgram @Amplify) (loopExample 10 20) funInterpCtx []

      result `shouldBeDistribution` [([FinV 10], 1.0)]

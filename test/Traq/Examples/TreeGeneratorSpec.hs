{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGeneratorSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives.Amplify.Prelude (Amplify)
import Traq.Primitives.Class
import Traq.ProtoLang

import Test.Hspec
import TestHelpers

loopExample :: forall ext sizeT. (Num sizeT, sizeT ~ SizeType ext) => sizeT -> sizeT -> Program ext
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
                                  { initial_args = ["acc"]
                                  , loop_body_fun = "AddWeight"
                                  }
                            ]
                      }
              }
        }
    ]

type Prim = Primitive (Amplify (Sym.Sym SizeT) Double)
type Prim' = Primitive (Amplify SizeT Double)

loadKnapsack ::
  -- | number of elements
  SizeT ->
  -- | bound on total weight
  SizeT ->
  -- | bound on total profit
  SizeT ->
  -- | number of iterations
  SizeT ->
  IO (Program Prim')
loadKnapsack n w p k = do
  Right prog <- parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.qb"
  return $
    prog
      & mapSize (Sym.subst "N" (Sym.con n))
      & mapSize (Sym.subst "W" (Sym.con w))
      & mapSize (Sym.subst "P" (Sym.con p))
      & mapSize (Sym.subst "K" (Sym.con k))
      & mapSize Sym.unSym

spec :: Spec
spec = do
  describe "Tree Generator Example" $ do
    it "parses" $ do
      expectRight =<< parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.qb"
      -- p `shouldBe` treeGeneratorExample (Sym.var "N") (Sym.var "W") (Sym.var "P")
      return ()

    it "typechecks" $ do
      p <-
        parseFromFile (programParser @Prim) "examples/tree_generator/tree_generator_01_knapsack.qb"
          >>= expectRight
      assertRight $ (typeCheckProg @Prim) p

    it "evaluates" $ do
      let n = 2
      prog <- loadKnapsack n 20 30 2
      let funInterpCtx =
            Ctx.fromList
              [ ("Capacity", const [FinV 20])
              , ("Profit", id)
              , ("Weight", id)
              ]
      let result = runProgram prog funInterpCtx []

      result
        `shouldBeDistribution` [ ([ArrV [FinV 0, FinV 1]], 0.8)
                               , ([ArrV [FinV 1, FinV 1]], 0.2)
                               ]

  describe "Loop example" $ do
    it "parses" $ do
      p <-
        parseFromFile (programParser @(Core (Sym.Sym SizeT) Double)) "examples/tree_generator/loop_example.qb"
          >>= expectRight
      p `shouldBe` loopExample (Sym.var "N") (Sym.var "W")

    it "evaluates" $ do
      let funInterpCtx = Ctx.singleton "AddWeight" (take 1)
      let result = runProgram @Core' (loopExample 10 20) funInterpCtx []

      result `shouldBeDistribution` [([FinV 10], 1.0)]

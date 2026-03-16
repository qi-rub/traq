{-# LANGUAGE TypeApplications #-}

module Traq.Examples.BasicSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import Traq.CPL
import qualified Traq.Compiler as Compiler
import qualified Traq.Compiler.Qiskit as Qiskit
import qualified Traq.Compiler.Qualtran as Qualtran
import Traq.Prelude
import qualified Traq.QPL as QPL

import Test.Hspec
import TestHelpers

loopExample :: forall ext size. (Num size, size ~ SizeType ext) => size -> size -> Program ext
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
                      , ret_names = ["acc"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["one"] $ BasicExprE $ ConstE (FinV 1) (Fin w)
                            , ExprS ["acc"] $
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

type SymCore = Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "Loop example" $ do
    it "parses" $ do
      p <-
        parseFromFile (programParser @SymCore) "examples/basic/loop_example.traq"
          >>= expectRight
      p `shouldBe` loopExample (Sym.var "N") (Sym.var "W")

    it "evaluates" $ do
      let funInterpCtx = Map.singleton "AddWeight" (take 1)
      let result = runProgram @Core' (loopExample 10 20) funInterpCtx []

      result `shouldBeDistribution` [([FinV 10], 1.0)]

    describe "Compile" $ do
      let load_prog = do
            loopExample @Core' 10 20
              & A.annotateProgWith (_exts A.annNoPrims)
              & expectRight

      before load_prog $ do
        it "lowers" $ \ex -> do
          assertRight $ Compiler.lowerProgram ex

        it "typechecks" $ \ex -> do
          ex_uqpl <- expectRight $ Compiler.lowerProgram ex
          assertRight $ QPL.typeCheckProgram ex_uqpl

        it "cost" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
          let cost_from_analysis = getCost $ A.costQProg ex
          getCost cost `shouldBeLE` cost_from_analysis

        xit "target-py-qualtran" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          _ <- evaluate $ force $ Qualtran.toPy ex_cqpl
          return ()

        xit "target-py-qiskit" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          _ <- evaluate $ force $ Qiskit.toPy ex_cqpl
          return ()

  fdescribe "for loop" $ do
    it "parses" $ do
      p <- parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
      assertRight p

    it "evaluates" $ do
      Right ex <- parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
      let ex' = mapSize (Sym.unSym . Sym.subst "N" 10 . Sym.subst "W" 20) ex
      let result = runProgram @Core' ex' mempty []

      result `shouldBeDistribution` [([FinV 10], 1.0)]

    xdescribe "Compile" $ do
      let load_prog =
            parseFromFile (programParser @SymCore) "examples/basic/for_loop.traq"
              >>= expectRight
                <&> mapSize (Sym.unSym . Sym.subst "N" 10 . Sym.subst "W" 20)
                <&> A.annotateProgWith (_exts A.annNoPrims)
              >>= expectRight

      beforeAll load_prog $ do
        it "lowers" $ \ex -> do
          assertRight $ Compiler.lowerProgram ex

        it "typechecks" $ \ex -> do
          ex_uqpl <- expectRight $ Compiler.lowerProgram ex
          assertRight $ QPL.typeCheckProgram ex_uqpl

        it "cost" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
          let cost_from_analysis = getCost $ A.costQProg ex
          getCost cost `shouldBeLE` cost_from_analysis

        xit "target-py-qualtran" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          _ <- evaluate $ force $ Qualtran.toPy ex_cqpl
          return ()

        xit "target-py-qiskit" $ \ex -> do
          ex_cqpl <- expectRight $ Compiler.lowerProgram ex
          _ <- evaluate $ force $ Qiskit.toPy ex_cqpl
          return ()

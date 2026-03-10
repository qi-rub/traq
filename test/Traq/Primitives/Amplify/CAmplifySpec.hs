module Traq.Primitives.Amplify.CAmplifySpec where

import qualified Data.Map as Map

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import Traq.Primitives.Amplify.CAmplify
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class

import Test.Hspec
import TestHelpers

type Prim size prec = Primitive (CAmplify size prec)

exampleProgram :: (Num size, Fractional prec) => size -> CPL.Program (Prim size prec)
exampleProgram n = CPL.Program [CPL.NamedFunDef "sampler" sampler, CPL.NamedFunDef "main" main_fun]
 where
  node_ty = CPL.Fin n

  -- EDSL hint: Traq.decl (node_ty ---> (CPL.tbool, node_ty))
  sampler =
    CPL.FunDef
      { CPL.param_types = [node_ty]
      , CPL.ret_types = [CPL.tbool, node_ty]
      , CPL.mbody = Nothing
      }

  -- EDSL hint: Traq.fn (() ---> (CPL.tbool, node_ty)) $ CPL.FunBody{...}
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
              CAmplify Amplify{p_min = 0.02}
      }

spec :: Spec
spec = describe "CAmplify" $ do
  describe "CAmplify cost example1" $ do
    let program = exampleProgram 10
    let p_min = 0.02

    it "calculates unitary cost correctly" $ do
      let eps = A.failProb (0.001 :: Double)

      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.costUProg prog'

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

    it "calculates quantum max cost correctly" $ do
      let eps = A.failProb (0.001 :: Double)

      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.costQProg prog'

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

    it "calculates quantum query cost correctly - sampler always succeeds" $ do
      let eps = A.failProb (0.001 :: Double)
      let funInterpCtx = Map.singleton "sampler" (const [CPL.toValue True, CPL.FinV 1])

      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.expCostQProg prog' [] funInterpCtx

      actualCost `shouldBe` 1.0

    it "calculates quantum query cost correctly - sampler always fails" $ do
      let eps = A.failProb (0.001 :: Double)
      let funInterpCtx = Map.singleton "sampler" (const [CPL.toValue False, CPL.FinV 1])

      prog' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.expCostQProg prog' [] funInterpCtx

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

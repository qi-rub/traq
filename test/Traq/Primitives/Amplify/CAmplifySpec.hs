module Traq.Primitives.Amplify.CAmplifySpec where

import qualified Data.Map as Map

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Primitives.Amplify.CAmplify
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type Prim sizeT precT = Primitive (CAmplify sizeT precT)

exampleProgram :: (Num sizeT, Fractional precT) => sizeT -> P.Program (Prim sizeT precT)
exampleProgram n = P.Program [P.NamedFunDef "sampler" sampler, P.NamedFunDef "main" main_fun]
 where
  node_ty = P.Fin n

  -- EDSL hint: Traq.decl (node_ty ---> (P.tbool, node_ty))
  sampler =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [P.tbool, node_ty]
      , P.mbody = Nothing
      }

  -- EDSL hint: Traq.fn (() ---> (P.tbool, node_ty)) $ P.FunBody{...}
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
          P.PrimCallE $
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

      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.costUProg prog'

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

    it "calculates quantum max cost correctly" $ do
      let eps = A.failProb (0.001 :: Double)

      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.costQProg prog'

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

    it "calculates quantum query cost correctly - sampler always succeeds" $ do
      let eps = A.failProb (0.001 :: Double)
      let funInterpCtx = Map.singleton "sampler" (const [P.toValue True, P.FinV 1])

      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.expCostQProg prog' [] funInterpCtx

      actualCost `shouldBe` 1.0

    it "calculates quantum query cost correctly - sampler always fails" $ do
      let eps = A.failProb (0.001 :: Double)
      let funInterpCtx = Map.singleton "sampler" (const [P.toValue False, P.FinV 1])

      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program
      let actualCost = getCost $ A.expCostQProg prog' [] funInterpCtx

      let expectedCost = _QMax eps p_min
      actualCost `shouldBe` expectedCost

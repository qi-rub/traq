module Traq.Primitives.Amplify.CAmplifySpec where

import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as P
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Primitives.Amplify.CAmplify
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

import Test.Hspec

type Prim sizeT precT = Primitive (CAmplify sizeT precT)

exampleProgram :: (Num sizeT, Fractional precT) => sizeT -> P.Program (Prim sizeT precT)
exampleProgram n = P.Program [P.NamedFunDef "sampler" sampler, P.NamedFunDef "main" main_fun]
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
          P.PrimCallE $
            Primitive [PartialFun{pfun_name = "sampler", pfun_args = [Just "x"]}] $
              CAmplify Amplify{p_min = 0.02}
      }

spec :: Spec
spec = describe "CAmplify" $ do
  describe "CAmplify cost example1" $ do
    let program = exampleProgram 10

    it "calculates unitary cost correctly" $ do
      let delta = P.l2NormError (0.001 :: Double)
      -- let expectedCost = 2 * _QryClassicalU ((delta / 2) / 2) p_min
      let actualCost = getCost $ P.unitaryQueryCost P.SplitSimple delta program

      actualCost `shouldBe` 2 * 1779.40444900044

    it "calculates quantum max cost correctly" $ do
      let eps = P.failProb (0.001 :: Double)

      -- let expectedCost = _QryClassicalMax (Sym.con eps / 4) p_min
      let actualCost = getCost $ P.quantumMaxQueryCost P.SplitSimple eps program

      actualCost `shouldBe` 410.5414937585894

    it "calculates quantum query cost correctly - sampler always succeeds" $ do
      let eps = P.failProb (0.001 :: Double)
      let funInterpCtx = Ctx.singleton "sampler" (\[_] -> [P.toValue True, P.FinV 1])

      let actualCost = getCost $ P.quantumQueryCost P.SplitSimple eps program funInterpCtx mempty

      actualCost `shouldBe` 1.0

    it "calculates quantum query cost correctly - sampler always fails" $ do
      let eps = P.failProb (0.001 :: Double)
      let funInterpCtx = Ctx.singleton "sampler" (\[_] -> [P.toValue False, P.FinV 1])

      -- let expectedCost = _QryClassicalMax (Sym.con eps / 2) p_min
      let actualCost = getCost $ P.quantumQueryCost P.SplitSimple eps program funInterpCtx mempty

      actualCost `shouldBe` 410.5414937585894

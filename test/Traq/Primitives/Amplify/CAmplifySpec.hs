module Traq.Primitives.Amplify.CAmplifySpec where

import qualified Data.Map as Map

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Primitives.Amplify.CAmplify (CAmplify (..), _QryClassicalMax, _QryClassicalU)
import Traq.Primitives.Amplify.Prelude (Amplify (..))
import qualified Traq.ProtoLang as P

import Test.Hspec

exampleProgram :: (Num sizeT) => sizeT -> P.Program CAmplify sizeT
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
            CAmplify (Amplify{sampler = "sampler", p_min = 0.02, sampler_args = ["x"]})
      }

spec :: Spec
spec = describe "CAmplify" $ do
  describe "CAmplify cost example1" $ do
    let samplerUnitaryCost = 100.0 :: Double
    let samplerClassicalCost = 5.0 :: Double
    let samplerQuantumCost = 5.0 :: Double
    let p_min = 0.02 :: Double
    let program = exampleProgram 10

    let unitaryTicks = Map.singleton "sampler" (Sym.con samplerUnitaryCost)
    let classicalTicks = Map.singleton "sampler" (Sym.con samplerClassicalCost)

    it "calculates unitary cost correctly" $ do
      let delta = Sym.var "delta" :: Sym.Sym Double
      let expectedCost = 2 * _QryClassicalU ((delta / 2) / 2) p_min * Sym.con samplerUnitaryCost
      let actualCost = P.unitaryQueryCost P.SplitSimple delta program unitaryTicks

      actualCost `shouldBe` expectedCost

    it "calculates quantum max cost correctly" $ do
      let eps = 0.2 :: Double

      let expectedCost = _QryClassicalMax (Sym.con eps / 4) p_min * Sym.con samplerQuantumCost
      let actualCost = P.quantumMaxQueryCost P.SplitSimple (Sym.con eps) program unitaryTicks classicalTicks

      actualCost `shouldBe` expectedCost

    it "calculates quantum query cost correctly - sampler always succeeds" $ do
      let eps = 0.2 :: Double
      let funInterpCtx = Ctx.singleton "sampler" (\[_] -> [P.toValue True, P.FinV 1])

      let expectedCost = Sym.con samplerClassicalCost
      let actualCost = P.quantumQueryCost P.SplitSimple (Sym.con eps) program unitaryTicks classicalTicks funInterpCtx mempty

      actualCost `shouldBe` expectedCost

    it "calculates quantum query cost correctly - sampler always fails" $ do
      let eps = 0.2 :: Double
      let funInterpCtx = Ctx.singleton "sampler" (\[_] -> [P.toValue False, P.FinV 1])

      let expectedCost = _QryClassicalMax (Sym.con eps / 2) p_min * Sym.con samplerClassicalCost
      let actualCost = P.quantumQueryCost P.SplitSimple (Sym.con eps) program unitaryTicks classicalTicks funInterpCtx mempty

      actualCost `shouldBe` expectedCost

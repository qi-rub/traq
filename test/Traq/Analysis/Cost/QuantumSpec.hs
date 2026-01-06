{-# LANGUAGE TypeApplications #-}

module Traq.Analysis.Cost.QuantumSpec (spec) where

import Prelude

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.Analysis
import qualified Traq.Analysis as A
import Traq.Analysis.Cost.UnitarySpec (unsafeParseProgram)
import Traq.Analysis.CostModel.QueryCost
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.Primitives.Search.Prelude as P
import Traq.Primitives.Search.QSearchCFNW (_EQSearch)
import Traq.ProtoLang
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type Ext = A.AnnFailProb (P.Primitive (P.QSearchCFNW SizeT Double))

exProg :: P.Program Ext
exProg =
  P.Program
    [ P.NamedFunDef "f" fn_f
    , P.NamedFunDef "main" fn_main
    ]
 where
  ty :: P.VarType SizeT
  ty = P.Fin 10

  fn_f, fn_main :: P.FunDef Ext
  fn_f =
    P.FunDef
      { P.param_types = [ty]
      , P.ret_types = [P.tbool]
      , P.mbody = Nothing
      }
  fn_main =
    P.FunDef
      { P.param_types = []
      , P.ret_types = []
      , P.mbody =
          Just
            P.FunBody
              { P.param_names = []
              , P.ret_names = []
              , P.body_stmt =
                  P.SeqS
                    [ P.ExprS
                        { P.rets = ["b"]
                        , P.expr =
                            P.PrimCallE $
                              A.AnnFailProb (A.failProb 0.1) $
                                P.Primitive [P.PartialFun "f" [Nothing]] $
                                  P.QSearchCFNW $
                                    P.PrimSearch P.AnyK ty
                        }
                    ]
              }
      }

spec :: Spec
spec = do
  describe "quantum cost analysis" $ do
    it "ex1" $ do
      let interps = Ctx.singleton "f" (\[x] -> [P.toValue $ (P.fromValue x :: Int) == 3])
      let c = getCost $ A.expCostQProg @(SimpleQueryCost Double) exProg [] interps
      c `shouldBe` 2 * _EQSearch (10 :: Int) 1 (A.failProb 0.1)
    it "probabilistic outcome" $ do
      prog <-
        expectRight $
          unsafeParseProgram . unlines $
            [ "declare Oracle(Fin<100>) -> Bool end"
            , "def main() -> Bool do"
            , "  res1 <-$ uniform : Fin<2>;"
            , "  return res"
            , "end"
            ]
      prog' <- expectRight $ annotateProgWith (_exts annNoPrims) prog
      let c = expCostQProg prog' [] default_ :: QueryCost Double
      c `shouldBe` default_

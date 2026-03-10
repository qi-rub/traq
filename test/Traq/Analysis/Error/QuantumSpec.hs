module Traq.Analysis.Error.QuantumSpec (spec) where

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.Primitives.Search.Prelude as P
import qualified Traq.Primitives.Search.QSearchCFNW as P

import Test.Hspec

type Ext = A.AnnFailProb (P.Primitive (P.QSearchCFNW SizeT Double))

exProg :: CPL.Program Ext
exProg =
  CPL.Program
    [ CPL.NamedFunDef "f" fn_f
    , CPL.NamedFunDef "main" fn_main
    ]
 where
  ty :: CPL.VarType SizeT
  ty = CPL.Fin 10

  fn_f, fn_main :: CPL.FunDef Ext
  fn_f =
    CPL.FunDef
      { CPL.param_types = [ty]
      , CPL.ret_types = [CPL.tbool]
      , CPL.mbody =
          Just
            CPL.FunBody
              { CPL.param_names = ["x"]
              , CPL.ret_names = ["b"]
              , CPL.body_stmt =
                  CPL.SeqS
                    [ CPL.ExprS
                        { CPL.rets = ["y"]
                        , CPL.expr = CPL.BasicExprE $ CPL.ConstE (CPL.FinV 5) ty
                        }
                    , CPL.ExprS
                        { CPL.rets = ["b"]
                        , CPL.expr = CPL.BasicExprE $ CPL.BinOpE CPL.EqOp (CPL.VarE "x") (CPL.VarE "y")
                        }
                    ]
              }
      }
  fn_main =
    CPL.FunDef
      { CPL.param_types = []
      , CPL.ret_types = []
      , CPL.mbody =
          Just
            CPL.FunBody
              { CPL.param_names = []
              , CPL.ret_names = []
              , CPL.body_stmt =
                  CPL.SeqS
                    [ CPL.ExprS
                        { CPL.rets = ["b"]
                        , CPL.expr =
                            CPL.PrimCallE $
                              A.AnnFailProb (A.failProb 0.1) $
                                P.Primitive [P.PartialFun "f" []] $
                                  P.QSearchCFNW $
                                    P.PrimSearch P.AnyK ty
                        }
                    ]
              }
      }

spec :: Spec
spec = do
  describe "error analysis" $ do
    it "ex1" $ do
      let eps = A.tvErrorQProg exProg
      eps `shouldBe` A.failProb 0.1

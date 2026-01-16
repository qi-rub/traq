module Traq.Analysis.Error.QuantumSpec (spec) where

import qualified Traq.Analysis as A
import Traq.Prelude
import qualified Traq.Primitives as P
import qualified Traq.Primitives.Search.Prelude as P
import qualified Traq.Primitives.Search.QSearchCFNW as P
import qualified Traq.ProtoLang as P

import Test.Hspec

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
      , P.mbody =
          Just
            P.FunBody
              { P.param_names = ["x"]
              , P.ret_names = ["b"]
              , P.body_stmt =
                  P.SeqS
                    [ P.ExprS
                        { P.rets = ["y"]
                        , P.expr = P.BasicExprE $ P.ConstE (P.FinV 5) ty
                        }
                    , P.ExprS
                        { P.rets = ["b"]
                        , P.expr = P.BasicExprE $ P.BinOpE P.EqOp (P.VarE "x") (P.VarE "y")
                        }
                    ]
              }
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

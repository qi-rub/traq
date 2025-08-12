{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.AmplifySpec (spec) where

import Test.Hspec
import TestHelpers
import Text.Parsec.String
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym
import Traq.Primitives.Amplify (QAmplify (..))
import qualified Traq.ProtoLang as P
import Traq.ProtoLang.Parser (programParser)
import qualified Traq.Utils.Printing as PP

exampleProgram :: (Num sizeT) => sizeT -> sizeT -> P.Program QAmplify sizeT
exampleProgram two n = P.Program{P.funCtx, P.stmt = P.SeqS [stmt1, stmt2, stmt3]}
 where
  bool_ty = P.Fin two
  node_ty = P.Fin n

  -- Sampler f of type (Fin<N>, Fin<N>) -> (Bool, Fin<N>)
  f =
    P.FunDef
      { P.param_types = [node_ty, node_ty]
      , P.ret_types = [bool_ty, node_ty]
      , P.mbody = Nothing
      }

  funCtx = Ctx.fromList [("f", f)]

  -- Statement 1: Assign constant 1 of type Fin<N> to x
  stmt1 =
    P.ExprS
      { P.rets = ["x1"]
      , P.expr =
          P.BasicExprE
            P.ConstE
              { P.val = P.FinV 1
              , P.ty = node_ty
              }
      }

  stmt2 =
    P.ExprS
      { P.rets = ["x2"]
      , P.expr =
          P.BasicExprE
            P.ConstE
              { P.val = P.FinV 2
              , P.ty = node_ty
              }
      }

  stmt3 =
    P.ExprS
      { P.rets = ["ok", "result"]
      , P.expr =
          P.FunCallE
            { P.fun_kind = P.PrimitiveCall QAmplify{sampler = "f", p_min = 0.02}
            , P.args = ["x1", "x2"]
            }
      }

spec :: Spec
spec = do
  describe "amplify example" $ do
    it "parses" $ do
      p <-
        parseFromFile
          (programParser)
          "examples/primitives/amplify.qb"
          >>= expectRight
      p `shouldBe` exampleProgram (Sym.con 2) (Sym.var "N")

    it "round-trips through print/parse" $ do
      let stmt = "ok, result <- @amplify[f, 0.02]\n(x1, x2);\n"
      let parsed = P.parseStmt @QAmplify stmt
      let printed = PP.toCodeString <$> parsed
      printed `shouldBe` Right stmt

    it "type checks" $ do
      let n = 20 :: Int
      assertRight $ P.typeCheckProg Ctx.empty (exampleProgram 2 n)

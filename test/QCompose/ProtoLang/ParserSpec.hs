module QCompose.ProtoLang.ParserSpec (spec) where

import qualified Data.Number.Symbolic as Sym

import Lens.Micro
import QCompose.Examples.MatrixSearch (matrixExample)
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Rewrites
import QCompose.ProtoLang.Syntax
import Test.Hspec
import Text.Parsec.String

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      parseStmt "x' <- x" `shouldBe` Right (SeqS [ExprS{rets = ["x'"], expr = VarE{arg = "x"}}])
    it "parses seq assign" $ do
      parseStmt "x' <- x; y' <- const 3 : Fin<4>"
        `shouldBe` Right
          ( SeqS
              [ ExprS{rets = ["x'"], expr = VarE{arg = "x"}}
              , ExprS{rets = ["y'"], expr = ConstE{val = 3, ty = Fin (Sym.con 4)}}
              ]
          )
    it "parses function call" $ do
      parseStmt "a, b <- f(x, y, z)"
        `shouldBe` Right
          ( SeqS
              [ ExprS
                  { expr = FunCallE{fun_kind = FunctionCall "f", args = ["x", "y", "z"]}
                  , rets = ["a", "b"]
                  }
              ]
          )
  describe "parse function def" $ do
    it "parses function" $ do
      parseFunDef
        ( unlines
            [ "def check_entry(i: Fin<N>, j: Fin<M>) do"
            , "e <- Oracle(i, j);"
            , "e' <- !e;"
            , "return e' : Bool"
            , "end"
            ]
        )
        `shouldBe` Right
          ( FunDef
              { fun_name = "check_entry"
              , param_binds = [("i", Fin (Sym.var "N")), ("j", Fin (Sym.var "M"))]
              , ret_binds = [("e'", Fin (Sym.con 2))]
              , body =
                  SeqS
                    [ ExprS{rets = ["e"], expr = FunCallE{fun_kind = OracleCall, args = ["i", "j"]}}
                    , ExprS{rets = ["e'"], expr = UnOpE{un_op = NotOp, arg = "e"}}
                    ]
              }
          )
  describe "parse file" $ do
    it "parses example" $ do
      e <- parseFromFile programParser "examples/matrix_search/matrix_search.qb"
      let e' = e <&> _stmt %~ rewriteOf _ast flattenSeq
      e'
        `shouldBe` Right
          (matrixExample (Sym.var "N") (Sym.var "M") (Fin (Sym.con 2)))

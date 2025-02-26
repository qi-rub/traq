module QCompose.ProtoLang.ParserSpec (spec) where

import QCompose.Basic
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Syntax
import Test.Hspec
import Text.Parsec.String

-- import Control.Monad (void)
-- import Lens.Micro (Traversal', over, rewriteOf)
-- import QCompose.Examples.MatrixSearch (matrixExample)
-- import QCompose.ProtoLang.Rewrites

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      parseStmt "x' <- x" `shouldBe` Right (SeqS [AssignS{ret = "x'", arg = "x"}])
    it "parses seq assign" $ do
      parseStmt "x' <- x; y' <- const 3 : Fin<4>"
        `shouldBe` Right
          ( SeqS
              [ AssignS{ret = "x'", arg = "x"}
              , ConstS{ret = "y'", val = 3, ty = Fin (Value 4)}
              ]
          )
    it "parses function call" $ do
      parseStmt "a, b <- f(x, y, z)"
        `shouldBe` Right
          ( SeqS
              [ FunCallS
                  { fun_kind = FunctionCall "f"
                  , args = ["x", "y", "z"]
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
              , params = [("i", Fin (SymExpr "N")), ("j", Fin (SymExpr "M"))]
              , rets = [("e'", Fin (Value 2))]
              , body =
                  SeqS
                    [ FunCallS{fun_kind = OracleCall, rets = ["e"], args = ["i", "j"]}
                    , UnOpS{un_op = NotOp, arg = "e", ret = "e'"}
                    ]
              }
          )
  describe "parse file" $ do
    xit "parses example" $ do
      e <- parseFromFile (program protoLangTokenParser) "examples/matrix_search/matrix_search.qb"
      -- e <- return $ over _stmt (rewriteOf _stmt flattenSeq) <$> e
      -- e
      --   `shouldBe` Right
      --     ( matrixExample (SymExpr "N") (SymExpr "M") (Fin (Value 2))
      --     )
      fmap (const ()) e `shouldBe` Right ()

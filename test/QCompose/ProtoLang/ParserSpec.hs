module QCompose.ProtoLang.ParserSpec (spec) where

import Control.Monad (void)
import QCompose.Basic
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Syntax
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      parseCode "x' <- x" `shouldBe` Right (SeqS [AssignS{ret = "x'", arg = "x"}])
    it "parses seq assign" $ do
      parseCode "x' <- x; y' <- const 3 : Fin<4>"
        `shouldBe` Right
          ( SeqS
              [ AssignS{ret = "x'", arg = "x"}
              , ConstS{ret = "y'", val = 3, ty = Fin (Value 4)}
              ]
          )
    it "parses function call" $ do
      parseCode "a, b <- f(x, y, z)"
        `shouldBe` Right
          ( SeqS
              [ FunCallS
                  { fun = "f"
                  , args = ["x", "y", "z"]
                  , rets = ["a", "b"]
                  }
              ]
          )
  describe "parse function def" $ do
    it "parses function" $ do
      let res =
            parse
              (funDef protoLangTokenParser)
              ""
              $ unlines
                [ "def check_entry(i: Fin<N>, j: Fin<M>) do"
                , "e <- Oracle(i, j);"
                , "e' <- !e;"
                , "return e' : Bool"
                , "end"
                ]
      void res `shouldBe` Right ()
  describe "parse file" $ do
    it "parses example" $ do
      e <- parseFromFile (program protoLangTokenParser) "examples/matrix_search/matrix_search.qb"
      void e `shouldBe` Right ()

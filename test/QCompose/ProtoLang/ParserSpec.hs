{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module QCompose.ProtoLang.ParserSpec (spec) where

import Data.Void (Void)
import qualified QCompose.Data.Symbolic as Sym
import Text.Parsec.String

import QCompose.Examples.MatrixSearch (matrixExample)
import QCompose.ProtoLang.Parser
import QCompose.ProtoLang.Rewrites
import QCompose.ProtoLang.Syntax
import QCompose.Utils.ASTRewriting

import QCompose.Primitives.Search.Symbolic

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      (parseStmt @Void) "x' <- x" `shouldBe` Right (SeqS [ExprS{rets = ["x'"], expr = BasicExprE "x"}])
    it "parses seq assign" $ do
      parseStmt @Void "x' <- x; y' <- const 3 : Fin<4>"
        `shouldBe` Right
          ( SeqS
              [ ExprS{rets = ["x'"], expr = BasicExprE "x"}
              , ExprS{rets = ["y'"], expr = BasicExprE ConstE{val = 3, ty = Fin (Sym.con 4)}}
              ]
          )
    it "parses function call" $ do
      parseStmt @Void "a, b <- f(x, y, z)"
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
      parseFunDef @Void
        ( unlines
            [ "def check_entry(i: Fin<N>, j: Fin<M>) do"
            , "  e <- Oracle(i, j);"
            , "  e' <- !e;"
            , "  return e' : Bool"
            , "end"
            ]
        )
        `shouldBe` Right
          NamedFunDef
            { fun_name = "check_entry"
            , fun_def =
                FunDef
                  { param_types = [Fin (Sym.var "N"), Fin (Sym.var "M")]
                  , mbody =
                      Just
                        FunBody
                          { param_names = ["i", "j"]
                          , body_stmt =
                              SeqS
                                [ ExprS{rets = ["e"], expr = FunCallE{fun_kind = FunctionCall "Oracle", args = ["i", "j"]}}
                                , ExprS{rets = ["e'"], expr = BasicExprE UnOpE{un_op = NotOp, operand = "e"}}
                                ]
                          , ret_names = ["e'"]
                          }
                  , ret_types = [Fin (Sym.con 2)]
                  }
            }

  describe "parse file" $ do
    it "parses example" $ do
      e <- parseFromFile (programParser @QSearchSym) "examples/matrix_search/matrix_search.qb" >>= expectRight
      let e' = rewriteAST flattenSeq e
      e' `shouldBe` matrixExample (Sym.var "N") (Sym.var "M") (Fin (Sym.con 2))

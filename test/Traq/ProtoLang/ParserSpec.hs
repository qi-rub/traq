{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Traq.ProtoLang.ParserSpec (spec) where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import Traq.Examples.MatrixSearch (matrixExampleS, mkMatrixExample)
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.Prelude
import Traq.ProtoLang.Lenses
import Traq.ProtoLang.Parser
import Traq.ProtoLang.Rewrites
import Traq.ProtoLang.Syntax
import Traq.Utils.ASTRewriting
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

roundTrip :: Program DefaultPrims' -> Expectation
roundTrip p = do
  code <- PP.toCodeStringM p
  p'_sym <- expectRight $ parseProgram @(DefaultPrims (Sym.Sym SizeT) Double) code
  let p' = p'_sym & mapSize Sym.unSym & rewriteAST flattenSeq
  p' `shouldBe` p
  return ()

type SymCore = Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "parse statement" $ do
    it "parses assign" $ do
      (parseStmt @SymCore) "x' <- x;" `shouldBe` Right (SeqS [ExprS{rets = ["x'"], expr = BasicExprE "x"}])
    it "parses seq assign" $ do
      parseStmt @SymCore "x' <- x; y' <- const 3 : Fin<4>;"
        `shouldBe` Right
          ( SeqS
              [ ExprS{rets = ["x'"], expr = BasicExprE "x"}
              , ExprS{rets = ["y'"], expr = BasicExprE ConstE{val = FinV 3, ty = Fin (Sym.con 4)}}
              ]
          )
    it "parses function call" $ do
      parseStmt @SymCore "a, b <- f(x, y, z);"
        `shouldBe` Right
          ( SeqS
              [ ExprS
                  { expr = FunCallE{fname = "f", args = ["x", "y", "z"]}
                  , rets = ["a", "b"]
                  }
              ]
          )
  describe "parse function def" $ do
    it "parses function" $ do
      parseFunDef @SymCore
        ( unlines
            [ "def check_entry(i: Fin<N>, j: Fin<M>) -> Bool do"
            , "  e <- Oracle(i, j);"
            , "  e' <- !e;"
            , "  return e'"
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
                                [ ExprS{rets = ["e"], expr = FunCallE{fname = "Oracle", args = ["i", "j"]}}
                                , ExprS{rets = ["e'"], expr = BasicExprE UnOpE{un_op = NotOp, operand = "e"}}
                                ]
                          , ret_names = ["e'"]
                          }
                  , ret_types = [Fin (Sym.con 2)]
                  }
            }

  describe "parse file" $ do
    it "parses example" $ do
      e <- parseFromFile (programParser @(DefaultPrims (Sym.Sym SizeT) Double)) "examples/matrix_search/matrix_search.qb" >>= expectRight
      let e' = rewriteAST flattenSeq e
      e' `shouldBe` mkMatrixExample (\ty f -> PrimCallE $ QAny $ Primitive [f] $ QSearchCFNW $ PrimSearch AnyK ty) (Sym.var "N") (Sym.var "M")

  describe "round trip" $ do
    it "matrixExampleS" $ do
      roundTrip (matrixExampleS 4 5)
      roundTrip (matrixExampleS 10 10)
    it "max_sat_hillclimb" $ do
      e_sym <- expectRight =<< parseFromFile (programParser @(DefaultPrims (Sym.Sym SizeT) Double)) "examples/hillclimb/max_sat_hillclimb.qb"
      let e =
            e_sym
              & mapSize (Sym.subst "n" 10)
              & mapSize (Sym.subst "W" 1000)
              & mapSize Sym.unSym
              & rewriteAST flattenSeq
      roundTrip e

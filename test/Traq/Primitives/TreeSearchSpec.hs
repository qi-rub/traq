{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.TreeSearchSpec (spec) where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CPL as CPL
import Traq.CPL.Parser (programParser)
import Traq.Prelude
import Traq.Primitives.TreeSearch

import Test.Hspec
import TestHelpers

exampleProgram :: size -> size -> CPL.Program (TreeSearch size prec)
exampleProgram n two =
  CPL.Program
    [ CPL.NamedFunDef "child" child
    , CPL.NamedFunDef "check" check
    , CPL.NamedFunDef "main" mainf
    ]
 where
  node_ty = CPL.Fin n
  bool_ty = CPL.Fin two

  check =
    CPL.FunDef
      { CPL.param_types = [node_ty]
      , CPL.ret_types = [bool_ty]
      , CPL.mbody = Nothing
      }

  child =
    CPL.FunDef
      { CPL.param_types = [node_ty]
      , CPL.ret_types = [node_ty, node_ty]
      , CPL.mbody = Nothing
      }

  mainf =
    CPL.FunDef
      { CPL.param_types = []
      , CPL.mbody =
          Just
            CPL.FunBody
              { CPL.param_names = []
              , CPL.body_stmt =
                  CPL.SeqS
                    [ CPL.ExprS
                        { CPL.rets = ["ok"]
                        , CPL.expr =
                            CPL.PrimCallE $
                              TreeSearch
                                { getChildren = "child"
                                , getChildrenArgs = []
                                , checkNode = "check"
                                , checkNodeArgs = []
                                }
                        }
                    ]
              , CPL.ret_names = ["ok"]
              }
      , CPL.ret_types = [bool_ty]
      }

spec :: Spec
spec = do
  describe "tree search example" $ do
    it "parses" $ do
      p <-
        parseFromFile
          (programParser @(TreeSearch (Sym.Sym SizeT) Double))
          "examples/primitives/treesearch.traq"
          >>= expectRight
      p `shouldBe` exampleProgram (Sym.var "N") (Sym.con 2)

    it "type checks" $ do
      let n = 20 :: Int
      assertRight $ CPL.typeCheckProg (exampleProgram n 2)

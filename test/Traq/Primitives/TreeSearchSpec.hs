{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.TreeSearchSpec (spec) where

import Text.Parsec.String

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Primitives.TreeSearch
import qualified Traq.ProtoLang as P
import Traq.ProtoLang.Parser (programParser)

import Test.Hspec
import TestHelpers

exampleProgram :: sizeT -> sizeT -> P.Program TreeSearch sizeT
exampleProgram n two =
  P.Program
    [ P.NamedFunDef "child" child
    , P.NamedFunDef "check" check
    , P.NamedFunDef "main" mainf
    ]
 where
  node_ty = P.Fin n
  bool_ty = P.Fin two

  check =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [bool_ty]
      , P.mbody = Nothing
      }

  child =
    P.FunDef
      { P.param_types = [node_ty]
      , P.ret_types = [node_ty, node_ty]
      , P.mbody = Nothing
      }

  mainf =
    P.FunDef
      { P.param_types = []
      , P.mbody =
          Just
            P.FunBody
              { P.param_names = []
              , P.body_stmt =
                  P.SeqS
                    [ P.ExprS
                        { P.rets = ["ok"]
                        , P.expr =
                            P.PrimCallE $
                              TreeSearch
                                { getChildren = "child"
                                , getChildrenArgs = []
                                , checkNode = "check"
                                , checkNodeArgs = []
                                }
                        }
                    ]
              , P.ret_names = ["ok"]
              }
      , P.ret_types = [bool_ty]
      }

spec :: Spec
spec = do
  describe "tree search example" $ do
    it "parses" $ do
      p <-
        parseFromFile
          (programParser @TreeSearch)
          "examples/primitives/treesearch.qb"
          >>= expectRight
      p `shouldBe` exampleProgram (Sym.var "N") (Sym.con 2)

    it "type checks" $ do
      let n = 20 :: Int
      assertRight $ P.typeCheckProg (exampleProgram n 2)

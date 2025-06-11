{-# LANGUAGE TypeApplications #-}

module QCompose.Primitives.TreeSearchSpec (spec) where

import Text.Parsec.String

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Symbolic as Sym
import qualified QCompose.ProtoLang as P
import QCompose.ProtoLang.Parser (programParser)

import QCompose.Primitives.TreeSearch

import Test.Hspec
import TestHelpers

exampleProgram :: sizeT -> sizeT -> P.Program TreeSearch sizeT
exampleProgram n two = P.Program{P.funCtx, P.stmt = P.SeqS [stmt]}
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

  funCtx = Ctx.fromList [("child", child), ("check", check)]
  stmt =
    P.ExprS
      { P.rets = ["ok"]
      , P.expr =
          P.FunCallE
            { P.fun_kind = P.PrimitiveCall TreeSearch{getChildren = "child", checkNode = "check"}
            , P.args = []
            }
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
      assertRight $ P.typeCheckProg Ctx.empty (exampleProgram n 2)

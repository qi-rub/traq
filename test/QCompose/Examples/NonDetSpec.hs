{-# LANGUAGE TypeApplications #-}

module QCompose.Examples.NonDetSpec (spec) where

import Data.Either (fromRight, isRight)
import Lens.Micro
import qualified QCompose.Data.Symbolic as Sym
import Text.Parsec.String (parseFromFile)

import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Tree as Tree

import QCompose.Prelude
import QCompose.Primitives (DefaultPrims)
import qualified QCompose.ProtoLang as P
import qualified QCompose.ProtoLang.Parser as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "SimpleExample" $ do
    let load = parseFromFile (PP.programParser @DefaultPrims) "examples/nondet.qb"
    it "parses" $ do
      mEx <- load
      assertRight mEx

    let load' = load <&> fromRight (error "parsing failed") <&> fmap Sym.unSym

    it "typechecks" $ do
      ex <- load'
      P.typeCheckProg Ctx.empty ex `shouldSatisfy` isRight

    it "all solutions" $ do
      ex <- load'
      let oracleF = const [1]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Tree.choice
          [ pure $ Ctx.fromList [("ok", 1), ("x", x)]
          | x <- [0 .. 9]
          ]

    it "no solutions" $ do
      ex <- load'
      let oracleF = const [0]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Tree.choice
          [ pure $ Ctx.fromList [("ok", 0), ("x", x)]
          | x <- [0 .. 9]
          ]

    it "some solutions" $ do
      ex <- load'
      let sols = [1, 4, 6] :: [Value]
      let oracleF = \[i] -> [P.boolToValue $ i `elem` sols]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Tree.choice
          [ pure $ Ctx.fromList [("ok", 1), ("x", x)]
          | x <- sols
          ]

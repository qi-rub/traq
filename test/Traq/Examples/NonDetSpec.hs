{-# LANGUAGE TypeApplications #-}

module Traq.Examples.NonDetSpec (spec) where

import Data.Either (fromRight, isRight)
import Lens.Micro.GHC
import Text.Parsec.String (parseFromFile)
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Probability as Prob

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P
import qualified Traq.ProtoLang.Parser as PP

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
      let oracleF = const [P.FinV 1]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Prob.uniform
          [ Ctx.fromList [("ok", P.FinV 1), ("x", x)]
          | x <- P.FinV <$> [0 .. 9]
          ]

    it "no solutions" $ do
      ex <- load'
      let oracleF = const [P.FinV 0]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Prob.uniform
          [ Ctx.fromList [("ok", P.FinV 0), ("x", x)]
          | x <- P.FinV <$> [0 .. 9]
          ]

    it "some solutions" $ do
      ex <- load'
      let sols = [1, 4, 6] :: [SizeT]
      let oracleF = \[P.FinV i] -> [P.toValue $ i `elem` sols]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) Ctx.empty

      out
        `shouldBe` Prob.uniform
          [ Ctx.fromList [("ok", P.FinV 1), ("x", P.FinV x)]
          | x <- sols
          ]

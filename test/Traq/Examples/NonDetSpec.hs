{-# LANGUAGE TypeApplications #-}

module Traq.Examples.NonDetSpec (spec) where

import Data.Either (fromRight, isRight)
import Text.Parsec.String (parseFromFile)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P
import qualified Traq.ProtoLang.Parser as PP

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "SimpleExample" $ do
    let load = parseFromFile (PP.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) "examples/primitives/nondet.traq"
    it "parses" $ do
      mEx <- load
      assertRight mEx

    let load' = load <&> fromRight (error "parsing failed") <&> P.mapSize Sym.unSym

    it "typechecks" $ do
      ex <- load'
      P.typeCheckProg ex `shouldSatisfy` isRight

    it "all solutions" $ do
      ex <- load'
      let oracleF = const [P.FinV 1]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 0.1 :: Double)
                               | x <- [0 .. 9]
                               , let sigma = [P.FinV 1, P.FinV x]
                               ]

    it "no solutions" $ do
      ex <- load'
      let oracleF = const [P.FinV 0]
      let out = P.runProgram ex (Ctx.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 0.1 :: Double)
                               | x <- [0 .. 9]
                               , let sigma = [P.FinV 0, P.FinV x]
                               ]

    it "some solutions" $ do
      ex <- load'
      let sols = [1, 4, 6] :: [SizeT]
      let oracleF [P.FinV i] = [P.toValue $ i `elem` sols]
          oracleF _ = error "invalid input"
      let out = P.runProgram @_ @Double ex (Ctx.singleton "Oracle" oracleF) []

      out
        `shouldBeDistribution` [ (sigma, 1 / 3 :: Double)
                               | x <- sols
                               , let sigma = [P.FinV 1, P.FinV x]
                               ]

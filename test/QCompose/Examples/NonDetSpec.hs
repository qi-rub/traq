module QCompose.Examples.NonDetSpec (spec) where

import Control.Monad.State
import Data.Either (fromRight, isRight)
import qualified Data.Map as Map
import qualified Data.Number.Symbolic as Sym
import Lens.Micro
import Text.Parsec.String (parseFromFile)

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.ProtoLang.Parser as PP
import QCompose.Utils.Tree

import Test.Hspec

spec :: Spec
spec = do
  describe "SimpleExample" $ do
    let load = parseFromFile PP.programParser "examples/nondet.qb"
    it "parses" $ do
      mEx <- load
      mEx `shouldSatisfy` isRight

    let load' =
          load
            <&> fromRight undefined
            <&> fmap Sym.unSym

    it "typechecks" $ do
      ex <- load'
      P.typeCheckProg Map.empty ex `shouldSatisfy` isRight

    it "all solutions" $ do
      ex <- load'
      let oracleF = const [1]
      let out = execStateT (P.execProgram ex oracleF) Map.empty

      out
        `shouldBe` choice
          [ pure $ Map.fromList [("ok", 1), ("x", x)]
          | x <- [0 .. 9]
          ]

    it "no solutions" $ do
      ex <- load'
      let oracleF = const [0]
      let out = execStateT (P.execProgram ex oracleF) Map.empty

      out
        `shouldBe` choice
          [ pure $ Map.fromList [("ok", 0), ("x", x)]
          | x <- [0 .. 9]
          ]

    it "some solutions" $ do
      ex <- load'
      let sols = [1, 4, 6] :: [Value]
      let oracleF = \[i] -> [boolToValue $ i `elem` sols]
      let out = execStateT (P.execProgram ex oracleF) Map.empty

      out
        `shouldBe` choice
          [ pure $ Map.fromList [("ok", 1), ("x", x)]
          | x <- sols
          ]

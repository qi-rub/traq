{-# LANGUAGE TypeApplications #-}

module QCompose.CQPL.LoweringSpec (spec) where

import qualified Data.Number.Symbolic as Sym
import Data.Void
import Lens.Micro
import qualified QCompose.Data.Context as Ctx

import QCompose.CQPL.Lowering
import QCompose.CQPL.Syntax
import QCompose.Prelude
import qualified QCompose.ProtoLang as P

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "lower simple programs" $ do
    let eps = 0.001 :: Double -- fail prob
    it "assign" $ do
      ex_ <-
        expectRight $
          P.parseProgram @Void $
            unlines
              [ "declare Oracle() -> Bool"
              , "x <- const 0 : Fin<10>"
              ]
      let ex = Sym.unSym <$> ex_
      (cq :: Program' SizeT, _) <- expectRight $ lowerProgram undefined Ctx.empty "Oracle" eps ex
      cq ^. to stmt `shouldBe` AssignS ["x"] (ConstE 0 (P.Fin 10))

{-# LANGUAGE TypeApplications #-}

module QCompose.CQPL.LoweringSpec (spec) where

import Data.Void
import Lens.Micro.GHC
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Symbolic as Sym

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
      (cq :: Program' SizeT Double, _) <- expectRight $ lowerProgram Ctx.empty "Oracle" eps ex
      cq ^. to stmt `shouldBe` AssignS ["x"] (ConstE 0 (P.Fin 10))

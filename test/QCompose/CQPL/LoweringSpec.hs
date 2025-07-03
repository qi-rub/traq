{-# LANGUAGE TypeApplications #-}

module QCompose.CQPL.LoweringSpec (spec) where

import qualified Data.Map as Map
import Data.Void
import Lens.Micro.GHC

import qualified QCompose.Data.Context as Ctx
import QCompose.Data.Default
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
      ex_ <- expectRight $ P.parseProgram @Void "x <- const 0 : Fin<10>"
      let ex = Sym.unSym <$> ex_
      let ticks = Map.singleton "Oracle" 1.0
      (cq :: Program' SizeT Double, _) <- expectRight $ lowerProgram default_ Ctx.empty ticks ticks eps ex
      cq ^. to stmt `shouldBe` SeqS [AssignS ["x"] (P.ConstE 0 (P.Fin 10))]

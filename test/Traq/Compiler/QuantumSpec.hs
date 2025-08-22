{-# LANGUAGE TypeApplications #-}

module Traq.Compiler.QuantumSpec (spec) where

import qualified Data.Map as Map
import Data.Void

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import Traq.CQPL.Syntax
import Traq.Compiler.Quantum
import Traq.Prelude
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

_ProcBodyC :: Traversal' (ProcBody sizeT costT) (CProcBody sizeT costT)
_ProcBodyC _focus (ProcBodyC cb) = ProcBodyC <$> _focus cb
_ProcBodyC _ b = pure b

spec :: Spec
spec = do
  describe "lower simple programs" $ do
    let eps = 0.001 :: Double -- fail prob
    it "assign" $ do
      ex_ <- expectRight $ P.parseProgram @Void "x <- const 0 : Fin<10>;"
      let ex = Sym.unSym <$> ex_
      let ticks = Map.singleton "Oracle" 1.0
      (cq :: Program SizeT Double) <- expectRight $ lowerProgram default_ Ctx.empty ticks ticks eps ex
      CProcBody{cproc_body_stmt} <-
        return $
          cq
            ^. to proc_defs
            . Ctx.at "main"
            . singular _Just
            . to proc_body
            . singular _ProcBodyC
      cproc_body_stmt `shouldBe` SeqS [AssignS ["x"] (P.ConstE (P.FinV 0) (P.Fin 10))]

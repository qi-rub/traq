{-# LANGUAGE TypeApplications #-}

module Traq.Compiler.QuantumSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as P
import Traq.CQPL.Syntax
import Traq.Compiler.Quantum
import Traq.Prelude
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

_ProcBodyC :: Traversal' (ProcBody sizeT) (CProcBody sizeT)
_ProcBodyC _focus (ProcBodyC cb) = ProcBodyC <$> _focus cb
_ProcBodyC _ b = pure b

type SymCore = P.Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "lower simple programs" $ do
    let eps = P.failProb (0.001 :: Double)
    it "assign" $ do
      ex_ <- expectRight $ P.parseProgram @SymCore "def main() -> () do x <- const 0 : Fin<10>; return end"
      let ex = P.mapSize Sym.unSym ex_
      (cq :: Program SizeT) <- expectRight $ lowerProgram default_ eps ex
      CProcBody{cproc_body_stmt} <-
        return $
          cq
            ^. to proc_defs
            . Ctx.at "main"
            . singular _Just
            . to proc_body
            . singular _ProcBodyC
      cproc_body_stmt `shouldBe` SeqS [AssignS ["x"] (P.ConstE (P.FinV 0) (P.Fin 10))]

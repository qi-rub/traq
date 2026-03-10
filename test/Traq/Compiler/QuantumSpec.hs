{-# LANGUAGE TypeApplications #-}

module Traq.Compiler.QuantumSpec (spec) where

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CPL as CPL
import Traq.Compiler.Quantum
import Traq.Prelude
import Traq.QPL.Syntax

import Test.Hspec
import TestHelpers

_ProcBodyC :: Traversal' (ProcBody size) (CProcBody size)
_ProcBodyC _focus (ProcBodyC cb) = ProcBodyC <$> _focus cb
_ProcBodyC _ b = pure b

type SymCore = CPL.Core (Sym.Sym SizeT) Double

spec :: Spec
spec = do
  describe "lower simple programs" $ do
    it "assign" $ do
      ex_ <- expectRight $ CPL.parseProgram @SymCore "def main() -> () do x <- const 0 : Fin<10>; return end"
      let ex = CPL.mapSize Sym.unSym ex_
      (cq :: Program SizeT) <- expectRight $ lowerProgram ex
      let Program cq_procs = cq
      CProcBody{cproc_body_stmt} <-
        return $
          cq_procs
            ^. to (Ctx.fromListWith proc_name)
            . Ctx.at "main"
            . singular _Just
            . to proc_body
            . singular _ProcBodyC
      cproc_body_stmt `shouldBe` SeqS [AssignS ["x"] (CPL.ConstE (CPL.FinV 0) (CPL.Fin 10))]

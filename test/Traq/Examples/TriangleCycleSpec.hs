{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type P = DefaultPrims (Sym.Sym SizeT) Double

examplePath :: String
examplePath = "examples/triangle_finding.traq"

loadExample :: IO (P.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (P.programParser @P) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con 8))
      & P.mapSize Sym.unSym
      & P.renameVars'

spec :: Spec
spec = describe "Triangle Cycle Finding" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ CQPL.typeCheckProgram ex_uqpl

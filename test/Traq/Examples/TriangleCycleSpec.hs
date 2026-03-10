{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TriangleCycleSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Compiler.Qualtran (toPy)
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

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ P.typeCheckProg ex

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

    it "cost" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ toPy ex_cqpl
      return ()

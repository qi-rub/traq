{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.GroverMeetsSimonSpec where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Compiler.Qualtran (toPy)
import Traq.Prelude
import Traq.Primitives
import qualified Traq.QPL as QPL

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/grover_meets_simon.traq"

type P = WorstCasePrims (Sym.Sym SizeT) Double

loadExample :: IO (CPL.Program (WorstCasePrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @P) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "n" (Sym.con 4))
      & CPL.mapSize Sym.unSym

spec :: Spec
spec = describe "Grover Meets Simon" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (CPL.programParser @P) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ QPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ toPy ex_cqpl
      return ()

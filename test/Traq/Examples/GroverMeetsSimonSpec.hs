{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.GroverMeetsSimonSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/cryptanalysis/grover_meets_simon.traq"

type P = WorstCasePrims (Sym.Sym SizeT) Double

loadExample :: IO (P.Program (WorstCasePrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (P.programParser @P) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "n" (Sym.con 4))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "Grover Meets Simon" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ P.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ CQPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWithErrorBudget eps ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

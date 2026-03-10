{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SteepMaxKSatSpec where

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

type Prims = WorstCasePrims SizeT Double
type SymPrims = WorstCasePrims (Sym.Sym SizeT) Double

examplePath :: String
examplePath = "examples/hillclimb/steep_max_sat.traq"

loadExample :: IO (CPL.Program Prims)
loadExample = do
  Right prog <- parseFromFile (CPL.programParser @SymPrims) examplePath
  return $
    prog
      & CPL.mapSize (Sym.subst "n" (Sym.con 4))
      & CPL.mapSize (Sym.subst "W" (Sym.con 8))
      & CPL.mapSize Sym.unSym

spec :: Spec
spec = describe "Steep max-k-sat" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (CPL.programParser @SymPrims) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ QPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- CPL.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ toPy ex_cqpl
      return ()

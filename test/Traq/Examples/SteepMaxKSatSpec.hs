{-# LANGUAGE TypeApplications #-}

module Traq.Examples.SteepMaxKSatSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (getCost))
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import TestHelpers

type Prims = WorstCasePrims SizeT Double
type SymPrims = WorstCasePrims (Sym.Sym SizeT) (Sym.Sym Double)

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
      & CPL.mapPrec Sym.unSym

spec :: Spec
spec = describe "Steep max-k-sat" $ do
  describe "parses" $ do
    it "file" $ do
      expectRight =<< parseFromFile (CPL.programParser @SymPrims) examplePath
      return ()

    it "roundtrip" $ do
      prog <- expectRight =<< parseFromFile (CPL.programParser @SymPrims) examplePath
      p <- expectRight $ CPL.parseProgram @SymPrims $ PP.toCodeString prog
      p `shouldBe` prog

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ CPL.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)
    let load_prog = do
          ex <- loadExample
          expectRight $ A.annotateProgWith (CPL._exts (A.annSinglePrim eps)) ex

    before load_prog $ do
      it "lowers" $ \ex -> do
        assertRight $ Compiler.lowerProgram ex

      it "typechecks" $ \ex -> do
        ex_uqpl <- expectRight $ Compiler.lowerProgram ex
        assertRight $ QPL.typeCheckProgram ex_uqpl

      it "cost" $ \ex -> do
        ex_cqpl <- expectRight $ Compiler.lowerProgram ex
        let cost = fst (QPL.programCost ex_cqpl) :: SimpleQueryCost Double
        let cost_from_analysis = getCost $ A.costQProg ex
        getCost cost `shouldBeLE` cost_from_analysis

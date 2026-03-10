{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ThreeRoundFeistelAttackSpec where

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
import Traq.Primitives
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

type Prims = WorstCasePrims SizeT Double
type SymPrims = WorstCasePrims (Sym.Sym SizeT) Double

examplePath :: String
examplePath = "examples/cryptanalysis/3_round_feistel.traq"

loadExample :: IO (P.Program Prims)
loadExample = do
  Right prog <- parseFromFile (P.programParser @SymPrims) examplePath
  let n = 4
  return $
    prog
      & P.mapSize (Sym.subst "n" (Sym.con n))
      & P.mapSize (Sym.subst "n_plus_1" (Sym.con (n + 1)))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "3 round feistel attack" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @SymPrims) examplePath
    return ()

  it "typechecks" $ do
    ex <- loadExample
    assertRight $ P.typeCheckProg ex

  describe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ CQPL.typeCheckProgram ex_uqpl

    it "cost" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      let cost = fst (CQPL.programCost ex_cqpl) :: SimpleQueryCost Double
      let cost_from_analysis = getCost $ A.costQProg ex'
      getCost cost `shouldBeLE` cost_from_analysis

    xit "target-py-qualtran" $ do
      ex <- P.renameVars' <$> loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_cqpl <- expectRight $ Compiler.lowerProgram ex'
      _ <- evaluate $ force $ toPy ex_cqpl
      return ()

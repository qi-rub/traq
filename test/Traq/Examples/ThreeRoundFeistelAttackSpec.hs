{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ThreeRoundFeistelAttackSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
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
  return $
    prog
      & P.mapSize (Sym.subst "n" (Sym.con 4))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "3 round feistel attack" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @SymPrims) examplePath
    return ()

  xdescribe "Compile" $ do
    let eps = A.failProb (0.0001 :: Double)

    it "lowers" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      assertRight $ Compiler.lowerProgram ex'

    it "typechecks" $ do
      ex <- loadExample
      ex' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) ex
      ex_uqpl <- expectRight $ Compiler.lowerProgram ex'
      assertRight $ CQPL.typeCheckProgram ex_uqpl

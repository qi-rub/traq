{-# LANGUAGE TypeApplications #-}

module Traq.Examples.ClusteringAlgorithmSpec where

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

examplePath :: String
examplePath = "examples/clustering_algorithm.traq"

loadExample :: IO (P.Program (DefaultPrims SizeT Double))
loadExample = do
  Right prog <- parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con 8))
      & P.mapSize (Sym.subst "M" (Sym.con 4))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "Clustering Algorithm" $ do
  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(DefaultPrims (Sym.Sym SizeT) Double)) examplePath
    return ()

  describe "Compile" $ do
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

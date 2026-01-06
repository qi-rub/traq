{-# LANGUAGE TypeApplications #-}

module Traq.Examples.EvanMansourSpec where

import Text.Parsec.String

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Simons.Quantum
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers (assertRight, expectRight)

examplePath :: String
examplePath = "examples/cryptanalysis/evan_mansour.qb"

type SPrim size = Primitive (SimonsFindXorPeriod size Double)

loadEvanMansour ::
  -- | bitsize @n@ of the inputs/outputs
  SizeT ->
  IO (P.Program (SPrim SizeT))
loadEvanMansour n = do
  Right prog <- parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
  return $
    prog
      & P.mapSize (Sym.subst "N" (Sym.con (2 ^ n)))
      & P.mapSize (Sym.subst "n" (Sym.con n))
      & P.mapSize Sym.unSym

spec :: Spec
spec = describe "FindXorPeriod" $ do
  -- bitsize
  let n = 7 :: SizeT

  -- p0 matching the code (TODO remove the redundancy)
  let p0 = 0.01 :: Double

  it "parses" $ do
    expectRight =<< parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
    return ()

  it "typechecks" $ do
    p <-
      parseFromFile (P.programParser @(SPrim (Sym.Sym SizeT))) examplePath
        >>= expectRight
    assertRight $ P.typeCheckProg p

  before (loadEvanMansour n) $ do
    it "calculates unitary cost correctly" $ \program -> do
      let eps = A.failProb (0.01 :: Double)
      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costUProg prog'
      let formulaCost = 2 + 4 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

    it "calculates quantum max cost correctly" $ \program -> do
      let eps = A.failProb (0.01 :: Double)
      prog' <- expectRight $ A.annotateProgWith (P._exts (A.annSinglePrim eps)) program

      let actualCost = getCost $ A.costQProg prog'
      let formulaCost = 2 + 4 * _SimonsQueries n p0 eps

      actualCost `shouldBe` formulaCost

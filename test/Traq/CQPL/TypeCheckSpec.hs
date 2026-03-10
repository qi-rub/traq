{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Traq.CQPL.TypeCheckSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CPL ((.&&.))
import qualified Traq.CPL as CPL
import qualified Traq.CQPL as CQPL

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typeCheckUStmt" $ do
    let tb = CPL.Fin (2 :: Int)
    let checker gamma s =
          runReaderT
            (CQPL.typeCheckUStmt @Int s)
            (default_ & CPL._typingCtx .~ gamma)
    describe "unitary embed" $ do
      it "AndOp" $
        assertRight $
          checker
            (Ctx.fromList [("a", tb), ("b", tb), ("c", tb)])
            CQPL.UnitaryS
              { CQPL.qargs = map CQPL.Arg ["a", "b", "c"]
              , CQPL.unitary = CQPL.RevEmbedU ["a0", "a1"] ("a0" .&&. "a1")
              }
      it "MultiOrOp" $
        forM_ [3 :: Int, 10] $ \n -> do
          let xs = ["a" <> show i | i <- [0 .. n]]
          assertRight $
            checker
              (Ctx.fromList $ map (,tb) ("out" : xs))
              CQPL.UnitaryS
                { CQPL.qargs = map CQPL.Arg xs ++ [CQPL.Arg "out"]
                , CQPL.unitary = CQPL.RevEmbedU xs (CPL.NAryE CPL.MultiOrOp $ map CPL.VarE xs)
                }

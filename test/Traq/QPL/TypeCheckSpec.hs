{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Traq.QPL.TypeCheckSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CPL ((.&&.))
import qualified Traq.CPL as CPL
import qualified Traq.QPL as QPL

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typeCheckUStmt" $ do
    let tb = CPL.Fin (2 :: Int)
    let checker gamma s =
          runReaderT
            (QPL.typeCheckUStmt @Int s)
            (default_ & CPL._typingCtx .~ gamma)
    describe "unitary embed" $ do
      it "AndOp" $
        assertRight $
          checker
            (Ctx.fromList [("a", tb), ("b", tb), ("c", tb)])
            QPL.UnitaryS
              { QPL.qargs = map QPL.Arg ["a", "b", "c"]
              , QPL.unitary = QPL.RevEmbedU ["a0", "a1"] ("a0" .&&. "a1")
              }
      it "MultiOrOp" $
        forM_ [3 :: Int, 10] $ \n -> do
          let xs = ["a" <> show i | i <- [0 .. n]]
          assertRight $
            checker
              (Ctx.fromList $ map (,tb) ("out" : xs))
              QPL.UnitaryS
                { QPL.qargs = map QPL.Arg xs ++ [QPL.Arg "out"]
                , QPL.unitary = QPL.RevEmbedU xs (CPL.NAryE CPL.MultiOrOp $ map CPL.VarE xs)
                }

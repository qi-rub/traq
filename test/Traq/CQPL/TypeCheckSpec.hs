{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Traq.CQPL.TypeCheckSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.Void (Void)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.CQPL as CQPL
import Traq.ProtoLang ((.&&.))
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typeCheckUStmt" $ do
    let tb = P.Fin (2 :: Int)
    let checker gamma s =
          runReaderT
            (CQPL.typeCheckUStmt @Void @Int @Float s)
            (default_ & P._typingCtx .~ gamma)
    describe "unitary embed" $ do
      it "AndOp" $
        assertRight $
          checker
            (Ctx.fromList [("a", tb), ("b", tb), ("c", tb)])
            CQPL.UnitaryS
              { CQPL.qargs = ["a", "b", "c"]
              , CQPL.unitary = CQPL.RevEmbedU ["a0", "a1"] ("a0" .&&. "a1")
              }
      it "MultiOrOp" $
        forM_ [3 :: Int, 10] $ \n -> do
          let xs = ["a" <> show i | i <- [0 .. n]]
          assertRight $
            checker
              (Ctx.fromList $ map (,tb) ("out" : xs))
              CQPL.UnitaryS
                { CQPL.qargs = xs ++ ["out"]
                , CQPL.unitary = CQPL.RevEmbedU xs (P.NAryE P.MultiOrOp $ map P.VarE xs)
                }

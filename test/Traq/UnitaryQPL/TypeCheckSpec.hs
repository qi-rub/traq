{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Traq.UnitaryQPL.TypeCheckSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Void (Void)
import Lens.Micro.GHC

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.ProtoLang ((.&&.))
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as U

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "typeCheckStmt" $ do
    let tb = P.Fin (2 :: Int)
    let checker gamma s = runMyReaderT (U.typeCheckStmt @Void @Int @Float s) (default_ & P._typingCtx .~ gamma)
    describe "unitary embed" $ do
      it "AndOp" $
        assertRight $
          checker
            (Ctx.fromList [("a", tb), ("b", tb), ("c", tb)])
            U.UnitaryS
              { U.args = ["a", "b", "c"]
              , U.unitary = U.RevEmbedU ["a0", "a1"] ("a0" .&&. "a1")
              }
      it "MultiOrOp" $
        forM_ [3, 10] $ \n -> do
          let xs = ["a" <> show i | i <- [0 .. n]]
          assertRight $
            checker
              (Ctx.fromList $ map (,tb) ("out" : xs))
              U.UnitaryS
                { U.args = xs ++ ["out"]
                , U.unitary = U.RevEmbedU xs (P.NAryE P.MultiOrOp $ map P.VarE xs)
                }

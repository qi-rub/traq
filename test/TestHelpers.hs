module TestHelpers where

import Data.Either
import Data.Maybe
import Test.Hspec

expectJust :: (Show a) => Maybe a -> IO a
expectJust v = do
  v `shouldSatisfy` isJust
  return $ fromJust v

expectRight :: (Show a, Show e) => Either e a -> IO a
expectRight v = do
  v `shouldSatisfy` isRight
  return $ fromRight undefined v

module TestHelpers where

import Data.Either
import Data.Maybe
import Test.Hspec

assertJust :: (Show a) => Maybe a -> IO ()
assertJust v = v `shouldSatisfy` isJust

expectJust :: (Show a) => Maybe a -> IO a
expectJust v = do
  assertJust v
  return $ fromJust v

assertRight :: (Show a, Show e) => Either e a -> IO ()
assertRight v = v `shouldSatisfy` isRight

expectRight :: (Show a, Show e) => Either e a -> IO a
expectRight v = do
  assertRight v
  return $ fromRight undefined v

-- | Check if two functions are same by functional extensionality.
shouldEqual :: (HasCallStack, Show a, Eq a) => (t -> a) -> (t -> a) -> (t -> Expectation)
shouldEqual f g x = f x `shouldBe` g x

infix 1 `shouldEqual`

module TestHelpers where

import Control.Monad (forM_)
import Data.Either
import Data.Maybe

import qualified Traq.Data.Probability as Prob

import Test.HUnit.Approx
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

-- | Check if a given probability distribution matches a list of weighted outcomes.
shouldBeDistribution ::
  forall probT m a.
  ( HasCallStack
  , Prob.MonadExp probT m
  , Fractional probT
  , Ord probT
  , Show probT
  , Show a
  , Eq a
  ) =>
  m a ->
  [(a, probT)] ->
  Expectation
shouldBeDistribution mu vals = do
  let epsilon = 1e-6 :: probT
  assertApproxEqual "mass" epsilon (sum $ map snd vals) $ Prob.mass mu
  forM_ vals $ \(x, p) ->
    assertApproxEqual ("Prob[" <> show x <> "]") epsilon p $ Prob.probabilityOf (== x) mu

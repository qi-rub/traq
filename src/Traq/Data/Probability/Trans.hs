{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Data.Probability.Trans where

import Control.Monad (join)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (MonadTrans (..))
import Lens.Micro.GHC

import Traq.Data.Probability.Class
import Traq.Data.Probability.Tree

-- | Probability monad transformer
newtype ProbT probT m a = ProbT {runProbT :: m (Distr probT a)}

instance (Functor m) => Functor (ProbT probT m) where
  fmap f = ProbT . fmap (fmap f) . runProbT

instance (Applicative m) => Applicative (ProbT probT m) where
  pure = ProbT . pure . pure

  (ProbT mtf) <*> (ProbT mta) = let mft = fmap (<*>) mtf in ProbT $ mft <*> mta

instance (Monad m) => Monad (ProbT probT m) where
  (ProbT mta) >>= f = ProbT $ mta >>= traverse (runProbT . f) <&> join

instance (MonadFail m) => MonadFail (ProbT probT m) where
  fail = ProbT . fail

instance MonadTrans (ProbT probT) where
  lift = ProbT . fmap pure

type Prob probT = ProbT probT Identity

runProb :: Prob probT a -> Distr probT a
runProb = runIdentity . runProbT

instance (Num probT, Monad m) => MonadProb probT (ProbT probT m) where
  choose = ProbT . fmap choose . traverse (\(p, x) -> (p,) <$> runProbT x)

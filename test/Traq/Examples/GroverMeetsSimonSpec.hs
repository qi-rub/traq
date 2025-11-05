{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Traq.Examples.GroverMeetsSimonSpec where

import Text.Parsec.String

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.Primitives (DefaultPrims)
import qualified Traq.ProtoLang as P

import Test.Hspec
import TestHelpers

examplePath :: String
examplePath = "examples/grover_meets_simon.qb"

type P = DefaultPrims (Sym.Sym SizeT) Double

-- data SimonsSearch sizeT precT
--     = Simon (SimonsFindXorPeriod (Sym.Sym sizeT) precT)
--     | Search (QSearchCFNW sizeT precT)
--     deriving (Eq, Show, Read)

-- instance P.Parseable (SimonsSearch sizeT Double) where
--   parseE tp = (SimonsFindXorPeriod <$> P.parseE tp) <|> (QSearchCFNW <$> P.parseE tp)

spec :: Spec
spec = describe "Grover Meets Simon" $ do
  xit "parses" $ do
    expectRight =<< parseFromFile (P.programParser @P) examplePath
    return ()

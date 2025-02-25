module QCompose.ProtoLang.Subroutine where

import QCompose.Basic

data Subroutine = Subroutine
  { name :: Ident
  , aliases :: [Ident]
  , nParams :: Int
  , validateNArgs :: Int -> Bool
  , validateNRets :: Int -> Bool
  }

qcontains :: Subroutine
qcontains = Subroutine{..}
  where
    name = "any"
    aliases = ["contains"]

    -- parameters: predicate
    nParams = 1

    validateNArgs _ = True
    validateNRets n = n == 1

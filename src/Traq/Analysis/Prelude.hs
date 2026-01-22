{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Analysis.Prelude where

import Data.Data (Proxy)

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude

class SizeToPrec ext where
  sizeToPrec :: Proxy ext -> SizeType ext -> PrecType ext

instance (Floating precT) => SizeToPrec Integer precT where sizeToPrec = fromIntegral
instance (Floating precT) => SizeToPrec Int precT where sizeToPrec = fromIntegral

instance (Show sizeT) => SizeToPrec (Sym.Sym sizeT) (Sym.Sym precT) where
  sizeToPrec s = Sym.var (show s)

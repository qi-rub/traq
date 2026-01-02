{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Analysis.Prelude where

import qualified Traq.Data.Symbolic as Sym

class SizeToPrec sizeT precT where
  sizeToPrec :: sizeT -> precT

instance (Floating precT) => SizeToPrec Integer precT where sizeToPrec = fromIntegral
instance (Floating precT) => SizeToPrec Int precT where sizeToPrec = fromIntegral

instance (Show sizeT) => SizeToPrec (Sym.Sym sizeT) (Sym.Sym precT) where
  sizeToPrec s = Sym.var (show s)

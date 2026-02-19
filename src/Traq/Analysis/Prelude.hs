{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Analysis.Prelude where

import qualified Traq.Data.Symbolic as Sym

class SizeToPrec size prec where
  sizeToPrec :: size -> prec

instance (Floating prec) => SizeToPrec Integer prec where sizeToPrec = fromIntegral
instance (Floating prec) => SizeToPrec Int prec where sizeToPrec = fromIntegral

instance (Show size) => SizeToPrec (Sym.Sym size) (Sym.Sym prec) where
  sizeToPrec s = Sym.var (show s)

{-# LANGUAGE FlexibleInstances #-}

module Traq.Analysis.Annotate.Symbolic (annSymEps, annSymEpsProg) where

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Symbolic as Sym

import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Error.Prelude
import Traq.Prelude
import Traq.ProtoLang

newEps :: (m ~ AnnotateMonad ext ext') => m (Sym.Sym p)
newEps = do
  i <- nextId
  return $ Sym.var $ "eps_" ++ show i

annSymEps ::
  ( m ~ AnnotateMonad ext (AnnFailProb ext)
  , PrecType ext ~ Sym.Sym p
  , Num p
  , Eq p
  ) =>
  ext ->
  m (AnnFailProb ext)
annSymEps p = do
  eps <- newEps
  pure $ AnnFailProb (failProb eps) p

annSymEpsProg ::
  forall p ext.
  (PrecType ext ~ Sym.Sym p, Num p, Eq p) =>
  Program ext ->
  Either String (Program (AnnFailProb ext))
annSymEpsProg = annotateProgWith $ \p -> do
  Program fs' <- traverseOf _exts annSymEps p
  fs_extra <- use _funCtx <&> funCtxToNamedFuns
  pure $ Program $ reverse fs_extra ++ fs'

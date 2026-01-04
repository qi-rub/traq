{-# LANGUAGE FlexibleInstances #-}

module Traq.Analysis.Annotate.Symbolic (annSymEps, annSymEpsProg) where

import Lens.Micro.Mtl

import qualified Traq.Data.Symbolic as Sym

import Traq.Analysis.Annotate.Prelude
import Traq.Analysis.Error.Prelude
import Traq.Prelude
import Traq.ProtoLang

newEps :: (m ~ AnnotateMonad ext) => m (Sym.Sym p)
newEps = do
  i <- nextId
  return $ Sym.var $ "eps_" ++ show i

-- | Assign a unique symbol eps to each primitive call.
class AnnotateSymEps f where
  annSymEps ::
    forall p ext m.
    ( m ~ AnnotateMonad ext
    , PrecType ext ~ Sym.Sym p
    , Num p
    , Eq p
    ) =>
    f ext ->
    m (f (AnnFailProb ext))

instance AnnotateSymEps Expr where
  annSymEps BasicExprE{basic_expr} = return BasicExprE{basic_expr}
  annSymEps RandomSampleE{distr_expr} = return RandomSampleE{distr_expr}
  annSymEps FunCallE{fname, args} = return FunCallE{fname, args}
  annSymEps (PrimCallE p) = do
    eps <- newEps
    return $ PrimCallE (AnnFailProb (failProb eps) p)
  annSymEps _ = error "TODO"

instance AnnotateSymEps Stmt where
  annSymEps ExprS{rets, expr} = do
    expr <- annSymEps expr
    return ExprS{rets, expr}
  annSymEps IfThenElseS{cond, s_true, s_false} = do
    s_true <- annSymEps s_true
    s_false <- annSymEps s_false
    return IfThenElseS{cond, s_true, s_false}
  annSymEps (SeqS ss) = SeqS <$> mapM annSymEps ss

instance AnnotateSymEps FunBody where
  annSymEps FunBody{param_names, ret_names, body_stmt} = do
    body_stmt <- annSymEps body_stmt
    return FunBody{param_names, ret_names, body_stmt}

instance AnnotateSymEps FunDef where
  annSymEps FunDef{param_types, ret_types, mbody = Nothing} = do
    return FunDef{param_types, ret_types, mbody = Nothing}
  annSymEps FunDef{param_types, ret_types, mbody = Just body} = do
    body <- annSymEps body
    return FunDef{param_types, ret_types, mbody = Just body}

instance AnnotateSymEps NamedFunDef where
  annSymEps NamedFunDef{fun_name, fun_def} = do
    fun_def <- annSymEps fun_def
    return NamedFunDef{fun_name, fun_def}

instance AnnotateSymEps Program where
  annSymEps (Program fs) = do
    fs' <- mapM annSymEps fs
    fs_extra <- use _funs
    return $ Program $ reverse fs_extra ++ fs'

annSymEpsProg ::
  forall p ext.
  (PrecType ext ~ Sym.Sym p, Num p, Eq p) =>
  Program ext ->
  Either String (Program (AnnFailProb ext))
annSymEpsProg = annotateProgWith annSymEps

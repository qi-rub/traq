{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Traq.CPL.Lenses (
  MapSize (..),
  MapPrec (..),
  _exts,
) where

import Traq.CPL.Syntax
import Traq.Prelude

import Lens.Micro.GHC

-- ================================================================================
-- Functor extension
-- ================================================================================

-- | Basic functor support for our AST
class MapSize ext where
  type MappedSize ext size'

  mapSize ::
    ( size ~ SizeType ext
    , size' ~ SizeType ext'
    , ext' ~ MappedSize ext size'
    , PrecType ext ~ PrecType ext'
    ) =>
    (size -> size') -> (ext -> ext')

instance MapSize (VarType size) where
  type MappedSize (VarType size) size' = VarType size'

  mapSize = fmap

instance MapSize (DistrExpr prec size) where
  type MappedSize (DistrExpr prec size) size' = DistrExpr prec size'

  mapSize f (UniformE ty) = UniformE (fmap f ty)
  mapSize _ (BernoulliE p) = BernoulliE p

instance (MapSize ext) => MapSize (Expr ext) where
  type MappedSize (Expr ext) size' = Expr (MappedSize ext size')

  mapSize f (BasicExprE e) = BasicExprE (fmap f e)
  mapSize f (RandomSampleE e) = RandomSampleE (mapSize f e)
  mapSize f (PrimCallE prim) = PrimCallE (mapSize f prim)
  mapSize _ FunCallE{..} = FunCallE{..}

instance (MapSize ext) => MapSize (Stmt ext) where
  type MappedSize (Stmt ext) size' = Stmt (MappedSize ext size')

  mapSize f ExprS{..} = ExprS{expr = mapSize f expr, ..}
  mapSize f IfThenElseS{..} = IfThenElseS{s_true = mapSize f s_true, s_false = mapSize f s_false, ..}
  mapSize f (SeqS ss) = SeqS $ map (mapSize f) ss
  mapSize f ForS{..} = ForS{loop_ty = fmap f loop_ty, loop_body = mapSize f loop_body, ..}

instance (MapSize ext) => MapSize (FunBody ext) where
  type MappedSize (FunBody ext) size' = FunBody (MappedSize ext size')

  mapSize f FunBody{..} = FunBody{body_stmt = mapSize f body_stmt, ..}

instance (MapSize ext) => MapSize (FunDef ext) where
  type MappedSize (FunDef ext) size' = FunDef (MappedSize ext size')

  mapSize f FunDef{..} =
    FunDef
      { param_types = map (fmap f) param_types
      , ret_types = map (fmap f) ret_types
      , mbody = fmap (mapSize f) mbody
      }

instance (MapSize ext) => MapSize (NamedFunDef ext) where
  type MappedSize (NamedFunDef ext) size' = NamedFunDef (MappedSize ext size')

  mapSize f NamedFunDef{..} = NamedFunDef{fun_def = mapSize f fun_def, ..}

instance (MapSize ext) => MapSize (Program ext) where
  type MappedSize (Program ext) size' = Program (MappedSize ext size')

  mapSize f (Program fs) = Program $ map (mapSize f) fs

instance MapSize (Core size prec) where
  type MappedSize (Core size prec) size' = Core size' prec

  mapSize _ = \case {}

-- ================================================================================
-- MapPrec: map over the precision type
-- ================================================================================

class MapPrec ext where
  type MappedPrec ext prec'

  mapPrec ::
    ( prec ~ PrecType ext
    , prec' ~ PrecType ext'
    , ext' ~ MappedPrec ext prec'
    , SizeType ext ~ SizeType ext'
    ) =>
    (prec -> prec') -> (ext -> ext')

instance MapPrec (DistrExpr prec size) where
  type MappedPrec (DistrExpr prec size) prec' = DistrExpr prec' size

  mapPrec _ (UniformE ty) = UniformE ty
  mapPrec f (BernoulliE p) = BernoulliE (f p)

instance (MapPrec ext) => MapPrec (Expr ext) where
  type MappedPrec (Expr ext) prec' = Expr (MappedPrec ext prec')

  mapPrec f (BasicExprE e) = BasicExprE e
  mapPrec f (RandomSampleE e) = RandomSampleE (mapPrec f e)
  mapPrec f (PrimCallE prim) = PrimCallE (mapPrec f prim)
  mapPrec _ FunCallE{..} = FunCallE{..}

instance (MapPrec ext) => MapPrec (Stmt ext) where
  type MappedPrec (Stmt ext) prec' = Stmt (MappedPrec ext prec')

  mapPrec f ExprS{..} = ExprS{expr = mapPrec f expr, ..}
  mapPrec f IfThenElseS{..} = IfThenElseS{s_true = mapPrec f s_true, s_false = mapPrec f s_false, ..}
  mapPrec f (SeqS ss) = SeqS $ map (mapPrec f) ss
  mapPrec f ForS{..} = ForS{loop_body = mapPrec f loop_body, ..}

instance (MapPrec ext) => MapPrec (FunBody ext) where
  type MappedPrec (FunBody ext) prec' = FunBody (MappedPrec ext prec')

  mapPrec f FunBody{..} = FunBody{body_stmt = mapPrec f body_stmt, ..}

instance (MapPrec ext) => MapPrec (FunDef ext) where
  type MappedPrec (FunDef ext) prec' = FunDef (MappedPrec ext prec')

  mapPrec f FunDef{..} = FunDef{mbody = fmap (mapPrec f) mbody, ..}

instance (MapPrec ext) => MapPrec (NamedFunDef ext) where
  type MappedPrec (NamedFunDef ext) prec' = NamedFunDef (MappedPrec ext prec')

  mapPrec f NamedFunDef{..} = NamedFunDef{fun_def = mapPrec f fun_def, ..}

instance (MapPrec ext) => MapPrec (Program ext) where
  type MappedPrec (Program ext) prec' = Program (MappedPrec ext prec')

  mapPrec f (Program fs) = Program $ map (mapPrec f) fs

instance MapPrec (Core size prec) where
  type MappedPrec (Core size prec) prec' = Core size prec'

  mapPrec _ = \case {}

-- ============================================================================
-- Simple traversal to focus on each `ext` in the program.
-- ============================================================================

-- | Assign a unique symbol eps to each primitive call.
class HasExts f where
  _exts ::
    forall ext ext'.
    (SizeType ext ~ SizeType ext', PrecType ext ~ PrecType ext') =>
    Traversal (f ext) (f ext') ext ext'

instance HasExts Expr where
  _exts _ BasicExprE{basic_expr} = pure BasicExprE{basic_expr}
  _exts _ RandomSampleE{distr_expr} = pure RandomSampleE{distr_expr}
  _exts _ FunCallE{fname, args} = pure FunCallE{fname, args}
  _exts focus (PrimCallE p) = PrimCallE <$> focus p

instance HasExts Stmt where
  _exts focus ExprS{rets, expr} = do
    expr <- _exts focus expr
    pure ExprS{rets, expr}
  _exts focus IfThenElseS{cond, s_true, s_false} = do
    s_true <- _exts focus s_true
    s_false <- _exts focus s_false
    pure IfThenElseS{cond, s_true, s_false}
  _exts focus (SeqS ss) = SeqS <$> traverse (_exts focus) ss
  _exts focus ForS{loop_ix, loop_ty, loop_body} = do
    loop_body <- _exts focus loop_body
    pure ForS{loop_ix, loop_ty, loop_body}

instance HasExts FunBody where
  _exts focus FunBody{param_names, ret_names, body_stmt} = do
    body_stmt <- _exts focus body_stmt
    pure FunBody{param_names, ret_names, body_stmt}

instance HasExts FunDef where
  _exts _ FunDef{param_types, ret_types, mbody = Nothing} = do
    pure FunDef{param_types, ret_types, mbody = Nothing}
  _exts focus FunDef{param_types, ret_types, mbody = Just body} = do
    body <- _exts focus body
    pure FunDef{param_types, ret_types, mbody = Just body}

instance HasExts NamedFunDef where
  _exts focus NamedFunDef{fun_name, fun_def} = do
    fun_def <- _exts focus fun_def
    pure NamedFunDef{fun_name, fun_def}

instance HasExts Program where
  _exts focus (Program fs) = Program <$> traverse (_exts focus) fs

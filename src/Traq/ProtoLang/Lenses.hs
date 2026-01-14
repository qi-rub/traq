{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Traq.ProtoLang.Lenses (
  MapSize (..),
  _exts,
) where

import Lens.Micro.GHC

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- ================================================================================
-- Functor extension
-- ================================================================================

-- | Basic functor support for our AST
class MapSize ext where
  type MappedSize ext size'

  mapSize :: (size ~ SizeType ext, size' ~ SizeType ext', ext' ~ MappedSize ext size') => (size -> size') -> (ext -> ext')

instance (MapSize ext) => MapSize (Expr ext) where
  type MappedSize (Expr ext) size' = Expr (MappedSize ext size')

  mapSize f (BasicExprE e) = BasicExprE (fmap f e)
  mapSize f (RandomSampleE e) = RandomSampleE (fmap f e)
  mapSize f (PrimCallE prim) = PrimCallE (mapSize f prim)
  mapSize _ FunCallE{..} = FunCallE{..}
  mapSize _ LoopE{..} = LoopE{..}

instance (MapSize ext) => MapSize (Stmt ext) where
  type MappedSize (Stmt ext) size' = Stmt (MappedSize ext size')

  mapSize f ExprS{..} = ExprS{expr = mapSize f expr, ..}
  mapSize f IfThenElseS{..} = IfThenElseS{s_true = mapSize f s_true, s_false = mapSize f s_false, ..}
  mapSize f (SeqS ss) = SeqS $ map (mapSize f) ss

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

-- ============================================================================
-- Simple traversal to focus on each `ext` in the program.
-- ============================================================================

-- | Assign a unique symbol eps to each primitive call.
class HasExts f where
  _exts ::
    forall ext ext'.
    (SizeType ext ~ SizeType ext') =>
    Traversal (f ext) (f ext') ext ext'

instance HasExts Expr where
  _exts _ BasicExprE{basic_expr} = pure BasicExprE{basic_expr}
  _exts _ RandomSampleE{distr_expr} = pure RandomSampleE{distr_expr}
  _exts _ FunCallE{fname, args} = pure FunCallE{fname, args}
  _exts focus (PrimCallE p) = PrimCallE <$> focus p
  _exts _ _ = error "TODO"

instance HasExts Stmt where
  _exts focus ExprS{rets, expr} = do
    expr <- _exts focus expr
    pure ExprS{rets, expr}
  _exts focus IfThenElseS{cond, s_true, s_false} = do
    s_true <- _exts focus s_true
    s_false <- _exts focus s_false
    pure IfThenElseS{cond, s_true, s_false}
  _exts focus (SeqS ss) = SeqS <$> traverse (_exts focus) ss

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.ProtoLang.Lenses (MapSize (..)) where

import Lens.Micro.GHC

import Traq.Prelude
import Traq.ProtoLang.Syntax
import Traq.Utils.ASTRewriting

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

-- --------------------------------------------------------------------------------
-- AST traversals
-- --------------------------------------------------------------------------------
instance HasAst (Stmt ext) where
  _ast focus (SeqS ss) = SeqS <$> traverse focus ss
  _ast focus (IfThenElseS cond s_true s_false) = IfThenElseS cond <$> focus s_true <*> focus s_false
  _ast _ s = pure s

instance HasStmt (Stmt ext) where
  type StmtOf (Stmt ext) = Stmt ext
  _stmt = id

instance HasStmt (FunBody ext) where
  type StmtOf (FunBody ext) = Stmt ext
  _stmt focus fbody@FunBody{body_stmt} = (\body_stmt' -> fbody{body_stmt = body_stmt'}) <$> focus body_stmt

instance HasStmt (FunDef ext) where
  type StmtOf (FunDef ext) = Stmt ext
  _stmt focus funDef@FunDef{mbody = Just body} = (\body' -> funDef{mbody = Just body'}) <$> _stmt focus body
  _stmt _ f = pure f

instance HasStmt (NamedFunDef ext) where
  type StmtOf (NamedFunDef ext) = Stmt ext
  _stmt focus f@NamedFunDef{fun_def} = _stmt focus fun_def <&> \fun_def' -> f{fun_def = fun_def'}

instance HasStmt (Program ext) where
  type StmtOf (Program ext) = Stmt ext
  _stmt focus (Program fs) = Program <$> traverse (_stmt focus) fs

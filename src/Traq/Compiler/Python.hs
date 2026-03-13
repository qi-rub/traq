{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Traq.Compiler.Python (
  Py,
  withEnv,
  py_indent,
  py_sanitizeIdent,
  py_typedArg,
  py_comment,
  py_pass,
  py_ifte,
  py_return,
  py_raise_s,
  py_notImplemented,
  py_def,
  py_class,
  py_decorator,
  py_property,
  py_metaParam,
  py_arg,
  py_expr,
  py_val,
  py_defaultVal,
  py_unOp,
  py_binOp,
  py_naryOp,
  toPyType,
) where

import Control.Monad.Reader (ReaderT (..))
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.CPL as CPL
import Traq.Prelude
import qualified Traq.QPL as QPL

-- ============================================================
-- Helpers for building python syntax
-- ============================================================

type Py ann = PP.Doc ann

withEnv :: (Monad m) => r -> ReaderT r m a -> ReaderT r' m a
withEnv r = magnify (lens (const r) const)

py_indent :: Py ann -> Py ann
py_indent = PP.indent tabwidth
 where
  tabwidth :: Int
  tabwidth = 4

-- ------------------------------------------------------------
-- variables and types
-- ------------------------------------------------------------
py_sanitizeIdent :: Ident -> Py ann
py_sanitizeIdent s = PP.pretty $ map conv s
 where
  conv '\'' = '_'
  conv c = c

py_typedArg :: String -> Py ann -> Py ann
py_typedArg x t = py_sanitizeIdent x <+> PP.colon <+> t

-- ------------------------------------------------------------
-- statements
-- ------------------------------------------------------------

py_comment :: String -> Py ann
py_comment c = PP.vsep $ lines c <&> \l -> PP.pretty $ "# " <> l

py_pass :: Py ann
py_pass = PP.pretty "pass"

py_ifte :: String -> Py ann -> Py ann -> Py ann
py_ifte b s_t s_f =
  PP.vsep
    [ PP.pretty $ "if (" <> b <> "):"
    , py_indent s_t
    , PP.pretty "else:"
    , py_indent s_f
    ]

py_return :: [Py ann] -> Py ann
py_return args = PP.pretty "return" <+> PP.hsep (PP.punctuate PP.comma args)

py_raise_s :: String -> Py ann
py_raise_s e = PP.pretty @String $ printf "raise Exception('%s')" e

py_notImplemented :: String -> Py ann
py_notImplemented msg = PP.pretty "NotImplementedError" <> PP.parens (PP.dquotes $ PP.pretty msg)

py_def :: Ident -> [Py ann] -> Py ann -> Py ann
py_def name params body =
  PP.vsep
    [ PP.pretty "def" <+> PP.pretty name <> PP.tupled params <> PP.pretty ":"
    , py_indent body
    ]

py_class :: Ident -> Ident -> Py ann -> Py ann
py_class name parent body =
  PP.vsep
    [ PP.pretty "class" <+> PP.pretty name <> PP.parens (PP.pretty parent) <> PP.colon
    , py_indent body
    ]

py_decorator :: String -> Py ann
py_decorator s = PP.pretty $ "@" <> s

py_property :: Ident -> Py ann -> Py ann
py_property name body =
  PP.vsep
    [ py_decorator "property"
    , py_def name [PP.pretty "self"] body
    ]

py_metaParam :: (Show size) => Either (CPL.MetaParam size) Ident -> Py ann
py_metaParam (Left (CPL.MetaName n)) = py_sanitizeIdent n
py_metaParam (Left (CPL.MetaSize s)) = PP.pretty (show s)
py_metaParam (Right name) = py_sanitizeIdent name

py_arg :: (Show size) => QPL.Arg size -> Py ann
py_arg (QPL.Arg x) = py_sanitizeIdent x
py_arg (QPL.ArrElemArg a i) = py_arg a <> PP.brackets (py_metaParam (Left i))

-- | Emit a python expression
py_expr :: (Show size) => CPL.BasicExpr size -> Py ann
py_expr CPL.VarE{var} = py_sanitizeIdent var
py_expr CPL.ParamE{param} = py_sanitizeIdent param
py_expr CPL.DefaultE{ty} = py_defaultVal ty
py_expr CPL.ConstE{val} = py_val val
py_expr CPL.UnOpE{un_op, operand} = py_unOp un_op <> PP.parens (py_expr operand)
py_expr CPL.BinOpE{bin_op, lhs, rhs} = PP.parens $ py_expr lhs <+> py_binOp bin_op <+> py_expr rhs
py_expr CPL.TernaryE{branch, lhs, rhs} = PP.parens $ py_expr lhs <+> PP.pretty "if" <+> py_expr branch <+> PP.pretty "else" <+> py_expr rhs
py_expr CPL.NAryE{op, operands} = py_naryOp op <> PP.tupled (map py_expr operands)
py_expr CPL.IndexE{arr_expr, ix_val} = py_expr arr_expr <> PP.brackets (PP.pretty (show ix_val))
py_expr CPL.DynIndexE{arr_expr, ix_expr} = py_expr arr_expr <> PP.brackets (py_expr ix_expr)
py_expr CPL.UpdateArrE{arr_expr, ix_expr, rhs} = error "TODO UpdateArrE"
py_expr CPL.ProjectE{tup_expr, tup_ix_val} = py_expr tup_expr <> PP.brackets (PP.pretty (show tup_ix_val))

py_val :: (Show size) => CPL.Value size -> Py ann
py_val (CPL.FinV n) = PP.pretty (show n)
py_val (CPL.ArrV vs) = PP.list (map py_val vs)
py_val (CPL.TupV vs) = PP.tupled (map py_val vs)

py_defaultVal :: (Show size) => CPL.VarType size -> Py ann
py_defaultVal (CPL.Fin _) = PP.pretty "0"
py_defaultVal (CPL.Bitvec _) = PP.pretty "0"
py_defaultVal (CPL.Arr n t) = PP.brackets (py_defaultVal t) <+> PP.pretty "*" <+> PP.pretty (show n)
py_defaultVal (CPL.Tup ts) = PP.tupled (map py_defaultVal ts)

py_unOp :: CPL.UnOp -> Py ann
py_unOp CPL.NotOp = PP.pretty "not "
py_unOp CPL.AnyOp = PP.pretty "any"
py_unOp CPL.AllOp = PP.pretty "all"
py_unOp CPL.MajOp = error "TODO MajOp"

py_binOp :: CPL.BinOp -> Py ann
py_binOp CPL.AddOp = PP.pretty "+"
py_binOp CPL.MulOp = PP.pretty "*"
py_binOp CPL.SubOp = PP.pretty "-"
py_binOp CPL.XorOp = PP.pretty "^"
py_binOp CPL.LEqOp = PP.pretty "<="
py_binOp CPL.LtOp = PP.pretty "<"
py_binOp CPL.AndOp = PP.pretty "and"
py_binOp CPL.EqOp = PP.pretty "=="
py_binOp CPL.VecSelectOp = error "TODO VecSelectOp"

py_naryOp :: CPL.NAryOp -> Py ann
py_naryOp CPL.MultiOrOp = PP.pretty "any"

-- ------------------------------------------------------------
-- Classical python types
-- ------------------------------------------------------------

toPyType :: CPL.VarType size -> Py ann
toPyType (CPL.Fin _) = PP.pretty "int"
toPyType (CPL.Bitvec _) = PP.pretty "int"
toPyType (CPL.Tup ts) = PP.pretty "tuple" <+> PP.brackets (PP.tupled (map toPyType ts))
toPyType (CPL.Arr _ t) = PP.pretty "list" <+> PP.brackets (toPyType t)

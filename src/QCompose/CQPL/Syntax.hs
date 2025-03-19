{-# LANGUAGE Rank2Types #-}

module QCompose.CQPL.Syntax (
  Expr (..),
  FunctionCall (..),
  Stmt (..),
  ProcDef (..),
  Program (..),
) where

import QCompose.Prelude

import QCompose.ProtoLang (VarType (..))
import qualified QCompose.ProtoLang as P

data Expr a
  = ConstE {val :: Value}
  | VarE {var :: Ident}
  | AddE {lhs, rhs :: Expr a}
  | LEqE {lhs, rhs :: Expr a}
  | AndE {lhs, rhs :: Expr a}
  | NotE {arg :: Expr a}

data FunctionCall = FunctionCall Ident | OracleCall

data Stmt a
  = SkipS
  | AssignS {args :: [Ident], expr :: Expr a}
  | RandomS {args :: [Ident], tys :: [VarType a]}
  | CallS {fun :: FunctionCall, args :: [Ident]}
  | CallUProcAndMeasS {uproc_id :: Ident}
  | SeqS [Stmt a]
  | IfThenElseS {cond :: Ident, s_true, s_false :: Stmt a}
  | ForS {iter_var :: Ident, iter_ty :: VarType a, loop_body :: Stmt a}

data ProcDef a = ProcDef
  { proc_name :: Ident
  , proc_params :: [(Ident, VarType a)]
  , proc_body :: Stmt a
  }

data Program a = Program
  { oracle_decl :: P.OracleDecl a
  , proc_defs :: [ProcDef a]
  , stmt :: Stmt a
  }

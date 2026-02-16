{-# LANGUAGE DefaultSignatures #-}

module Traq.ProtoLang.Vars (
  -- * Program Variables
  VarSet,
  HasFreeVars (..),
  freeVars,
  outVars,
  allNamesP,

  -- * Checks
  checkVarsUnique,

  -- * Renaming
  RenameVars (..),
  addOnePrefix,
) where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import GHC.Generics

import Lens.Micro.GHC

import Traq.Prelude
import Traq.ProtoLang.Syntax

type VarSet = Set.Set Ident

-- | Any language term/expr/... that has a (possibly empty) set of free variables.
class HasFreeVars t where
  freeVarsList :: t -> [Ident]
  default freeVarsList :: (Generic t, GHasFreeVars (Rep t)) => t -> [Ident]
  freeVarsList t = gfreeVarsList (from t)

-- Generics
class GHasFreeVars f where
  gfreeVarsList :: f t -> [Ident]

instance GHasFreeVars U1 where
  gfreeVarsList U1 = []

instance (GHasFreeVars a, GHasFreeVars b) => GHasFreeVars (a :+: b) where
  gfreeVarsList (L1 a) = gfreeVarsList a
  gfreeVarsList (R1 b) = gfreeVarsList b

instance (GHasFreeVars a, GHasFreeVars b) => GHasFreeVars (a :*: b) where
  gfreeVarsList (a :*: b) = gfreeVarsList a ++ gfreeVarsList b

instance (GHasFreeVars a) => GHasFreeVars (M1 i c a) where
  gfreeVarsList (M1 x) = gfreeVarsList x

instance (HasFreeVars a) => GHasFreeVars (K1 i a) where
  gfreeVarsList (K1 x) = freeVarsList x

instance HasFreeVars Void where freeVarsList = absurd

freeVars :: (HasFreeVars t) => t -> VarSet
freeVars = Set.fromList . freeVarsList

instance HasFreeVars (Core size prec) where
  freeVarsList = \case {}

instance HasFreeVars (BasicExpr sizeT) where
  freeVarsList VarE{var} = [var]
  freeVarsList ConstE{} = []
  freeVarsList ParamE{} = []
  freeVarsList DefaultE{} = []
  freeVarsList UnOpE{operand} = freeVarsList operand
  freeVarsList BinOpE{lhs, rhs} = concatMap freeVarsList [lhs, rhs]
  freeVarsList TernaryE{branch, lhs, rhs} = concatMap freeVarsList [branch, lhs, rhs]
  freeVarsList NAryE{operands} = concatMap freeVarsList operands
  freeVarsList IndexE{arr_expr} = freeVarsList arr_expr
  freeVarsList DynIndexE{arr_expr, ix_expr} = concatMap freeVarsList [arr_expr, ix_expr]
  freeVarsList UpdateArrE{arr_expr, ix_expr, rhs} = concatMap freeVarsList [arr_expr, ix_expr, rhs]
  freeVarsList ProjectE{tup_expr} = freeVarsList tup_expr

instance HasFreeVars (DistrExpr sizeT) where
  freeVarsList UniformE{} = []
  freeVarsList BernoulliE{} = []

-- | The set of free (unbound) variables in an expression
instance (HasFreeVars ext) => HasFreeVars (Expr ext) where
  freeVarsList BasicExprE{basic_expr} = freeVarsList basic_expr
  freeVarsList RandomSampleE{distr_expr} = freeVarsList distr_expr
  freeVarsList FunCallE{args} = args
  freeVarsList PrimCallE{prim} = freeVarsList prim
  freeVarsList LoopE{initial_args} = initial_args

-- | The set of free (unbound) variables
instance (HasFreeVars ext) => HasFreeVars (Stmt ext) where
  freeVarsList IfThenElseS{cond, s_true, s_false} = cond : freeVarsList s_true ++ freeVarsList s_false
  freeVarsList (SeqS ss) = Set.elems $ Set.fromList (concatMap freeVarsList ss) Set.\\ outVars (SeqS ss)
  freeVarsList ExprS{expr} = freeVarsList expr

-- | The set of generated output variables
outVars :: Stmt ext -> VarSet
outVars IfThenElseS{s_true, s_false} = Set.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = Set.unions $ map outVars ss
outVars ExprS{rets} = Set.fromList rets

-- | All variables in a statement
allVars :: (HasFreeVars ext) => Stmt ext -> VarSet
allVars s = freeVars s `Set.union` outVars s

allNamesF :: (HasFreeVars ext) => FunBody ext -> VarSet
allNamesF FunBody{param_names, ret_names, body_stmt} =
  Set.unions [allVars body_stmt, Set.fromList param_names, Set.fromList ret_names]

-- | Get all the variables of a program
allNamesP :: (HasFreeVars ext) => Program ext -> VarSet
allNamesP (Program fs) =
  foldr (Set.union . allNamesF) mempty $ mapMaybe (mbody . fun_def) fs

-- | Check if a program has unique variable names
checkVarsUnique :: (HasFreeVars ext) => Program ext -> Bool
checkVarsUnique (Program fs) =
  isJust . foldM combine Set.empty $ all_fun_names
 where
  all_fun_names = fs & mapMaybe (mbody . fun_def) & map allNamesF

  combine :: VarSet -> VarSet -> Maybe VarSet
  combine u v = do
    guard $ Set.disjoint u v
    return $ Set.union u v

-- | Make all variable names in the program unique
class RenameVars p where
  renameVars :: Ident -> p -> p

  renameVars' :: p -> p
  renameVars' = renameVars ""

addOnePrefix :: Ident -> Ident -> Ident
addOnePrefix prefix s = prefix ++ "_" ++ s

addPrefix :: Ident -> [Ident] -> [Ident]
addPrefix prefix = map (addOnePrefix prefix)

instance RenameVars (BasicExpr size) where
  renameVars pref (VarE v) = VarE $ addOnePrefix pref v
  renameVars pref (UnOpE op e) = UnOpE op $ renameVars pref e
  renameVars pref (BinOpE op e1 e2) = BinOpE op (renameVars pref e1) (renameVars pref e2)
  renameVars pref (IndexE a i) = IndexE (renameVars pref a) i
  renameVars pref (DynIndexE a i) = DynIndexE (renameVars pref a) (renameVars pref i)
  renameVars pref (UpdateArrE a i v) = UpdateArrE (renameVars pref a) (renameVars pref i) (renameVars pref v)
  renameVars pref (ProjectE tup i) = ProjectE (renameVars pref tup) i
  renameVars pref (TernaryE{branch, lhs, rhs}) =
    TernaryE
      { branch = renameVars pref branch
      , lhs = renameVars pref lhs
      , rhs = renameVars pref rhs
      }
  renameVars _ e = e

instance (RenameVars ext) => RenameVars (Expr ext) where
  renameVars prefix BasicExprE{basic_expr} = BasicExprE $ renameVars prefix basic_expr
  renameVars _ e@RandomSampleE{} = e
  renameVars prefix e@FunCallE{args} = e{args = addPrefix prefix args}
  renameVars prefix PrimCallE{prim} = PrimCallE $ renameVars prefix prim
  renameVars prefix e@LoopE{initial_args} = e{initial_args = addPrefix prefix initial_args}

instance (RenameVars ext) => RenameVars (Stmt ext) where
  renameVars prefix ExprS{rets, expr} =
    ExprS
      { rets = addPrefix prefix rets
      , expr = renameVars prefix expr
      }
  renameVars prefix (SeqS ss) = SeqS $ map (renameVars prefix) ss
  renameVars _ _ = error "unsupported"

instance (RenameVars ext) => RenameVars (FunBody ext) where
  renameVars prefix FunBody{param_names, ret_names, body_stmt} =
    FunBody
      { param_names = addPrefix prefix param_names
      , ret_names = addPrefix prefix ret_names
      , body_stmt = renameVars prefix body_stmt
      }

instance (RenameVars ext) => RenameVars (FunDef ext) where
  renameVars prefix f@FunDef{mbody = Just body} = f{mbody = Just $ renameVars prefix body}
  renameVars _ f = f

instance (RenameVars ext) => RenameVars (NamedFunDef ext) where
  renameVars _ f@NamedFunDef{fun_name, fun_def} = f{fun_def = renameVars fun_name fun_def}

instance (RenameVars ext) => RenameVars (Program ext) where
  renameVars pref (Program fs) = Program $ map (renameVars pref) fs

instance RenameVars (Core size prec) where
  renameVars _ = \case {}

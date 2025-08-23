{-# LANGUAGE DefaultSignatures #-}

module Traq.ProtoLang.Vars (
  VarSet,
  HasFreeVars (..),
  freeVars,
  checkVarsUnique,
  allNamesP,
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

instance HasFreeVars (BasicExpr sizeT) where
  freeVarsList VarE{var} = [var]
  freeVarsList ConstE{} = []
  freeVarsList ParamE{} = []
  freeVarsList UnOpE{operand} = freeVarsList operand
  freeVarsList BinOpE{lhs, rhs} = concatMap freeVarsList [lhs, rhs]
  freeVarsList TernaryE{branch, lhs, rhs} = concatMap freeVarsList [branch, lhs, rhs]
  freeVarsList NAryE{operands} = concatMap freeVarsList operands
  freeVarsList IndexE{arr_expr} = freeVarsList arr_expr
  freeVarsList DynIndexE{arr_expr, ix_expr} = concatMap freeVarsList [arr_expr, ix_expr]
  freeVarsList UpdateArrE{arr_expr, ix_expr, rhs} = concatMap freeVarsList [arr_expr, ix_expr, rhs]
  freeVarsList ProjectE{tup_expr} = freeVarsList tup_expr

-- | The set of free (unbound) variables in an expression
instance (HasFreeVars primT) => HasFreeVars (Expr primT sizeT) where
  freeVarsList BasicExprE{basic_expr} = freeVarsList basic_expr
  freeVarsList FunCallE{args} = args
  freeVarsList PrimCallE{prim} = freeVarsList prim
  freeVarsList UniformRandomE{} = []
  freeVarsList BiasedCoinE{} = []

-- | The set of free (unbound) variables
instance (HasFreeVars primT) => HasFreeVars (Stmt primT sizeT) where
  freeVarsList IfThenElseS{cond, s_true, s_false} = cond : freeVarsList s_true ++ freeVarsList s_false
  freeVarsList (SeqS ss) = Set.elems $ Set.fromList (concatMap freeVarsList ss) Set.\\ outVars (SeqS ss)
  freeVarsList ExprS{expr} = freeVarsList expr

-- | The set of generated output variables
outVars :: Stmt primT sizeT -> VarSet
outVars IfThenElseS{s_true, s_false} = Set.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = Set.unions $ map outVars ss
outVars ExprS{rets} = Set.fromList rets

-- | All variables in a statement
allVars :: (HasFreeVars primT) => Stmt primT sizeT -> VarSet
allVars s = freeVars s `Set.union` outVars s

allNamesF :: (HasFreeVars primT) => FunBody primT sizeT -> VarSet
allNamesF FunBody{param_names, ret_names, body_stmt} =
  Set.unions [allVars body_stmt, Set.fromList param_names, Set.fromList ret_names]

-- | Get all the variables of a program
allNamesP :: (HasFreeVars primT) => Program primT sizeT -> VarSet
allNamesP (Program fs) =
  foldr (Set.union . allNamesF) mempty $ mapMaybe (mbody . fun_def) fs

-- | Check if a program has unique variable names
checkVarsUnique :: (HasFreeVars primT) => Program primT sizeT -> Bool
checkVarsUnique (Program fs) =
  isJust . foldM combine Set.empty $ all_fun_names
 where
  all_fun_names = fs & mapMaybe (mbody . fun_def) & map allNamesF

  combine :: VarSet -> VarSet -> Maybe VarSet
  combine u v = do
    guard $ Set.disjoint u v
    return $ Set.union u v

{- | Make all variable names in the program unique
 makeVarsUnique :: Program primT sizeT -> Program primT sizeT
 makeVarsUnique = error "TODO"
-}

module Traq.ProtoLang.Vars (
  VarSet,
  HasFreeVars (..),
  checkVarsUnique,
  allNamesP,
) where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Void (Void, absurd)

import Lens.Micro.GHC

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang.Syntax

type VarSet = Set.Set Ident

-- | Any language term/expr/... that has a (possibly empty) set of free variables.
class HasFreeVars t where
  freeVars :: t -> VarSet
  freeVars = Set.fromList . freeVarsList

  freeVarsList :: t -> [Ident]
  freeVarsList = Set.elems . freeVars

  {-# MINIMAL freeVars | freeVarsList #-}

instance HasFreeVars Void where freeVars = absurd

instance HasFreeVars (BasicExpr sizeT) where
  freeVars VarE{var} = Set.singleton var
  freeVars ConstE{} = Set.empty
  freeVars ParamE{} = Set.empty
  freeVars UnOpE{operand} = freeVars operand
  freeVars BinOpE{lhs, rhs} = Set.unions $ map freeVars [lhs, rhs]
  freeVars TernaryE{branch, lhs, rhs} = Set.unions $ map freeVars [branch, lhs, rhs]
  freeVars NAryE{operands} = Set.unions $ map freeVars operands
  freeVars IndexE{arr_expr} = freeVars arr_expr
  freeVars DynIndexE{arr_expr, ix_expr} = Set.unions $ map freeVars [arr_expr, ix_expr]
  freeVars UpdateArrE{arr_expr, ix_expr, rhs} = Set.unions $ map freeVars [arr_expr, ix_expr, rhs]
  freeVars ProjectE{tup_expr} = freeVars tup_expr

-- | The set of free (unbound) variables in an expression
instance (HasFreeVars primT) => HasFreeVars (Expr primT sizeT) where
  freeVars BasicExprE{basic_expr} = freeVars basic_expr
  freeVars FunCallE{args} = Set.fromList args
  freeVars PrimCallE{prim} = freeVars prim
  freeVars UniformRandomE{} = Set.empty
  freeVars BiasedCoinE{} = Set.empty

-- | The set of free (unbound) variables
instance (HasFreeVars primT) => HasFreeVars (Stmt primT sizeT) where
  freeVars IfThenElseS{cond, s_true, s_false} = Set.unions [Set.singleton cond, freeVars s_true, freeVars s_false]
  freeVars (SeqS ss) = Set.unions (map freeVars ss) Set.\\ outVars (SeqS ss)
  freeVars ExprS{expr} = freeVars expr

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
allNamesP Program{funCtx, stmt} =
  foldr Set.union (allVars stmt) $
    funCtx & Ctx.elems & mapMaybe mbody & map allNamesF

-- | Check if a program has unique variable names
checkVarsUnique :: (HasFreeVars primT) => Program primT sizeT -> Bool
checkVarsUnique Program{funCtx, stmt} =
  isJust . foldM combine Set.empty $ allVars stmt : all_fun_names
 where
  all_fun_names = funCtx & Ctx.elems & mapMaybe mbody & map allNamesF

  combine :: VarSet -> VarSet -> Maybe VarSet
  combine u v = do
    guard $ Set.disjoint u v
    return $ Set.union u v

{- | Make all variable names in the program unique
 makeVarsUnique :: Program primT sizeT -> Program primT sizeT
 makeVarsUnique = error "TODO"
-}

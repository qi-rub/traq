module QCompose.ProtoLang.Vars (
  freeVarsBE,
  VarSet,
  freeVars,
  -- outVars,
  -- allVars,
  checkVarsUnique,
  allNamesP,
) where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Lens.Micro.GHC

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang.Syntax

type VarSet = Set.Set Ident

freeVarsBE :: BasicExpr sizeT -> VarSet
freeVarsBE VarE{var} = Set.singleton var
freeVarsBE ConstE{} = Set.empty
freeVarsBE UnOpE{operand} = freeVarsBE operand
freeVarsBE BinOpE{lhs, rhs} = Set.unions $ map freeVarsBE [lhs, rhs]
freeVarsBE TernaryE{branch, lhs, rhs} = Set.unions $ map freeVarsBE [branch, lhs, rhs]
freeVarsBE NAryE{operands} = Set.unions $ map freeVarsBE operands

-- | The set of free (unbound) variables in an expression
freeVarsE :: Expr primT sizeT -> VarSet
freeVarsE BasicExprE{basic_expr} = freeVarsBE basic_expr
freeVarsE FunCallE{args} = Set.fromList args

-- | The set of free (unbound) variables
freeVars :: Stmt primT sizeT -> VarSet
freeVars IfThenElseS{cond, s_true, s_false} = Set.unions [Set.singleton cond, freeVars s_true, freeVars s_false]
freeVars (SeqS ss) = Set.unions (map freeVars ss) Set.\\ outVars (SeqS ss)
freeVars ExprS{expr} = freeVarsE expr

-- | The set of generated output variables
outVars :: Stmt primT sizeT -> VarSet
outVars IfThenElseS{s_true, s_false} = Set.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = Set.unions $ map outVars ss
outVars ExprS{rets} = Set.fromList rets

-- | All variables in a statement
allVars :: Stmt primT sizeT -> VarSet
allVars s = freeVars s `Set.union` outVars s

allNamesF :: FunBody primT sizeT -> VarSet
allNamesF FunBody{param_names, ret_names, body_stmt} =
  Set.unions [allVars body_stmt, Set.fromList param_names, Set.fromList ret_names]

-- | Get all the variables of a program
allNamesP :: Program primT sizeT -> VarSet
allNamesP Program{funCtx, stmt} =
  foldr Set.union (allVars stmt) $
    funCtx & Ctx.elems & mapMaybe mbody & map allNamesF

-- | Check if a program has unique variable names
checkVarsUnique :: Program primT sizeT -> Bool
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

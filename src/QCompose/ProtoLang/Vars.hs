module QCompose.ProtoLang.Vars where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import QCompose.Prelude
import QCompose.ProtoLang.Syntax

type VarSet = Set.Set Ident

-- | The set of free (unbound) variables in an expression
freeVarsE :: Expr a -> VarSet
freeVarsE VarE{arg} = Set.singleton arg
freeVarsE ConstE{} = Set.empty
freeVarsE UnOpE{arg} = Set.singleton arg
freeVarsE BinOpE{lhs, rhs} = Set.fromList [lhs, rhs]
freeVarsE FunCallE{args} = Set.fromList args

-- | The set of free (unbound) variables
freeVars :: Stmt a -> VarSet
freeVars IfThenElseS{cond, s_true, s_false} = Set.unions [Set.singleton cond, freeVars s_true, freeVars s_false]
freeVars (SeqS ss) = Set.unions (map freeVars ss) Set.\\ outVars (SeqS ss)
freeVars ExprS{expr} = freeVarsE expr

-- | The set of generated output variables
outVars :: Stmt a -> VarSet
outVars IfThenElseS{s_true, s_false} = Set.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = Set.unions $ map outVars ss
outVars ExprS{rets} = Set.fromList rets

-- | All variables in a statement
allVars :: Stmt a -> VarSet
allVars s = freeVars s `Set.union` outVars s

-- | Get all the variables of a program
allNamesP :: Program a -> VarSet
allNamesP Program{funCtx = FunCtx{fun_defs}, stmt} =
  foldr Set.union Set.empty $ allVars stmt : map allFunNames fun_defs
 where
  allFunNames :: FunDef a -> VarSet
  allFunNames FunDef{param_binds, body} =
    Set.unions [allVars body, Set.fromList $ map fst param_binds]

-- | Check if a program has unique variable names
checkVarsUnique :: Program a -> Bool
checkVarsUnique Program{funCtx = FunCtx{fun_defs}, stmt} =
  isJust . foldM combine Set.empty $ allVars stmt : map allFunVars fun_defs
 where
  allFunVars :: FunDef a -> VarSet
  allFunVars FunDef{param_binds, body} = Set.union (allVars body) (Set.fromList $ map fst param_binds)

  combine :: VarSet -> VarSet -> Maybe VarSet
  combine u v = do
    guard $ Set.disjoint u v
    return $ Set.union u v

-- | Make all variable names in the program unique
makeVarsUnique :: Program a -> Program a
makeVarsUnique = error "TODO"

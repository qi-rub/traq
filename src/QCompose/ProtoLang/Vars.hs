module QCompose.ProtoLang.Vars where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust)
import qualified Data.Set as S
import QCompose.Prelude
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

-- | The set of free (unbound) variables in an expression
freeVarsE :: Expr a -> VarSet
freeVarsE VarE{arg} = S.singleton arg
freeVarsE ConstE{} = S.empty
freeVarsE UnOpE{arg} = S.singleton arg
freeVarsE BinOpE{lhs, rhs} = S.fromList [lhs, rhs]
freeVarsE FunCallE{args} = S.fromList args

-- | The set of free (unbound) variables
freeVars :: Stmt a -> VarSet
freeVars IfThenElseS{cond, s_true, s_false} = S.unions [S.singleton cond, freeVars s_true, freeVars s_false]
freeVars (SeqS ss) = S.unions (map freeVars ss) S.\\ outVars (SeqS ss)
freeVars ExprS{expr} = freeVarsE expr

-- | The set of generated output variables
outVars :: Stmt a -> VarSet
outVars IfThenElseS{s_true, s_false} = S.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = S.unions $ map outVars ss
outVars ExprS{rets} = S.fromList rets

-- | All variables in a program
allVars :: Stmt a -> VarSet
allVars s = freeVars s `S.union` outVars s

-- | Check if a program has unique variable names
checkVarsUnique :: Program a -> Bool
checkVarsUnique Program{funCtx = FunCtx{fun_defs}, stmt} =
  isJust . foldM combine S.empty $ allVars stmt : map allFunVars fun_defs
 where
  allFunVars :: FunDef a -> VarSet
  allFunVars FunDef{param_binds, body} = S.union (allVars body) (S.fromList $ map fst param_binds)

  combine :: VarSet -> VarSet -> Maybe VarSet
  combine u v = do
    guard $ S.disjoint u v
    return $ S.union u v

-- | Make all variable names in the program unique
makeVarsUnique :: Program a -> Program a
makeVarsUnique = error "TODO"

module QCompose.ProtoLang.Vars where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust)
import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

-- | The set of free (unbound) variables
freeVars :: Stmt a -> VarSet
freeVars AssignS{arg} = S.singleton arg
freeVars ConstS{} = S.empty
freeVars UnOpS{arg} = S.singleton arg
freeVars BinOpS{lhs, rhs} = S.fromList [lhs, rhs]
freeVars FunCallS{args} = S.fromList args
freeVars IfThenElseS{cond, s_true, s_false} = S.unions [S.singleton cond, freeVars s_true, freeVars s_false]
freeVars (SeqS ss) = S.unions (map freeVars ss) S.\\ outVars (SeqS ss)

-- | The set of generated output variables
outVars :: Stmt a -> VarSet
outVars AssignS{ret} = S.singleton ret
outVars ConstS{ret} = S.singleton ret
outVars UnOpS{ret} = S.singleton ret
outVars BinOpS{ret} = S.singleton ret
outVars FunCallS{rets} = S.fromList rets
outVars IfThenElseS{s_true, s_false} = S.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = S.unions $ map outVars ss

-- | All variables in a program
allVars :: Stmt a -> VarSet
allVars s = freeVars s `S.union` outVars s

-- | Check if a program has unique variable names
checkVarsUnique :: Program a -> Bool
checkVarsUnique Program{funCtx = FunCtx{funDefs}, stmt} =
  isJust . foldM combine S.empty $ allVars stmt : map allFunVars funDefs
  where
    allFunVars :: FunDef a -> VarSet
    allFunVars FunDef{params, body} = S.union (allVars body) (S.fromList $ map fst params)

    combine :: VarSet -> VarSet -> Maybe VarSet
    combine u v = do
      guard $ S.disjoint u v
      return $ S.union u v

-- | Make all variable names in the program unique
makeVarsUnique :: Program a -> Program a
makeVarsUnique = error "TODO"

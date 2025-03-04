module QCompose.ProtoLang.Vars where

import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

-- | The set of free (unbound) variables
freeVars :: Stmt a -> VarSet
freeVars AssignS{..} = S.singleton arg
freeVars ConstS{..} = S.empty
freeVars UnOpS{..} = S.singleton arg
freeVars BinOpS{..} = S.fromList [lhs, rhs]
freeVars FunCallS{..} = S.fromList args
freeVars IfThenElseS{..} = S.unions [S.singleton cond, freeVars s_true, freeVars s_false]
freeVars (SeqS ss) = S.unions (map freeVars ss) S.\\ outVars (SeqS ss)

-- | The set of generated output variables
outVars :: Stmt a -> VarSet
outVars AssignS{..} = S.singleton ret
outVars ConstS{..} = S.singleton ret
outVars UnOpS{..} = S.singleton ret
outVars BinOpS{..} = S.singleton ret
outVars FunCallS{..} = S.fromList rets
outVars IfThenElseS{..} = S.unions [outVars s_true, outVars s_false]
outVars (SeqS ss) = S.unions $ map outVars ss

-- | All variables in a program
allVars :: Stmt a -> VarSet
allVars s = freeVars s `S.union` outVars s

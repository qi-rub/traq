module QCompose.ProtoLang.Vars where

import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

inputVars :: Stmt a -> VarSet
inputVars AssignS{..} = S.singleton arg
inputVars ConstS{..} = S.empty
inputVars UnOpS{..} = S.singleton arg
inputVars BinOpS{..} = S.fromList [lhs, rhs]
inputVars OracleS{..} = S.fromList args
inputVars FunCallS{..} = error "TODO"
inputVars ContainsS{..} = S.fromList args
inputVars SearchS{..} = S.fromList args
inputVars IfThenElseS{..} = error "TODO"
inputVars (SeqS []) = S.empty
inputVars (SeqS [s]) = inputVars s
inputVars (SeqS (s : ss)) = inputVars s `S.union` (inputVars (SeqS ss) S.\\ outputVars s)

outputVars :: Stmt a -> VarSet
outputVars AssignS{..} = S.singleton ret
outputVars ConstS{..} = S.singleton ret
outputVars UnOpS{..} = S.singleton ret
outputVars BinOpS{..} = S.singleton ret
outputVars OracleS{..} = S.fromList rets
outputVars FunCallS{..} = error "TODO"
outputVars ContainsS{..} = S.singleton ok
outputVars SearchS{..} = S.fromList [sol, ok]
outputVars IfThenElseS{..} = error "TODO"
outputVars (SeqS []) = S.empty
outputVars (SeqS [s]) = outputVars s
outputVars (SeqS (s : ss)) = (outputVars s S.\\ inputVars (SeqS ss)) `S.union` outputVars (SeqS ss)

allVars :: Stmt a -> VarSet
allVars s = S.union (inputVars s) (outputVars s)

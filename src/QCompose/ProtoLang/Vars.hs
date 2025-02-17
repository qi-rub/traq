module QCompose.ProtoLang.Vars where

import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

inputVars :: Stmt a -> VarSet
inputVars SAssign{..} = S.singleton arg
inputVars SConst{..} = S.empty
inputVars SUnOp{..} = S.singleton arg
inputVars SBinOp{..} = S.fromList [lhs, rhs]
inputVars SOracle{..} = S.fromList args
inputVars SFunCall{..} = error "TODO"
inputVars SContains{..} = S.fromList args
inputVars SSearch{..} = S.fromList args
inputVars SIfTE{..} = error "TODO"
inputVars (SSeq []) = S.empty
inputVars (SSeq [s]) = inputVars s
inputVars (SSeq (s : ss)) = inputVars s `S.union` (inputVars (SSeq ss) S.\\ outputVars s)

outputVars :: Stmt a -> VarSet
outputVars SAssign{..} = S.singleton ret
outputVars SConst{..} = S.singleton ret
outputVars SUnOp{..} = S.singleton ret
outputVars SBinOp{..} = S.singleton ret
outputVars SOracle{..} = S.fromList rets
outputVars SFunCall{..} = error "TODO"
outputVars SContains{..} = S.singleton ok
outputVars SSearch{..} = S.fromList [sol, ok]
outputVars SIfTE{..} = error "TODO"
outputVars (SSeq []) = S.empty
outputVars (SSeq [s]) = outputVars s
outputVars (SSeq (s : ss)) = (outputVars s S.\\ inputVars (SSeq ss)) `S.union` outputVars (SSeq ss)

allVars :: Stmt a -> VarSet
allVars s = S.union (inputVars s) (outputVars s)

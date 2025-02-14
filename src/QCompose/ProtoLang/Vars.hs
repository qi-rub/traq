module QCompose.ProtoLang.Vars where

import qualified Data.Set as S
import QCompose.Basic
import QCompose.ProtoLang.Syntax

type VarSet = S.Set Ident

inputVars :: Stmt -> VarSet
inputVars SAssign{..} = S.singleton arg
inputVars SConst{..} = S.empty
inputVars SUnOp{..} = S.singleton arg
inputVars SBinOp{..} = S.fromList [lhs, rhs]
inputVars SOracle{..} = S.fromList args
inputVars SFunCall{..} = error "TODO"
inputVars SContains{..} = S.fromList args
inputVars SSearch{..} = S.fromList args
inputVars SIfTE{..} = error "TODO"
inputVars (SSeq s1 s2) = inputVars s1 `S.union` (inputVars s2 S.\\ outputVars s1)

outputVars :: Stmt -> VarSet
outputVars SAssign{..} = S.singleton ret
outputVars SConst{..} = S.singleton ret
outputVars SUnOp{..} = S.singleton ret
outputVars SBinOp{..} = S.singleton ret
outputVars SOracle{..} = S.fromList rets
outputVars SFunCall{..} = error "TODO"
outputVars SContains{..} = S.singleton ok
outputVars SSearch{..} = S.fromList [sol, ok]
outputVars SIfTE{..} = error "TODO"
outputVars (SSeq s1 s2) = (outputVars s1 S.\\ inputVars s2) `S.union` outputVars s2

allVars :: Stmt -> VarSet
allVars s = S.union (inputVars s) (outputVars s)

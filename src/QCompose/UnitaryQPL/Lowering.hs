module QCompose.UnitaryQPL.Lowering where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.UnitaryQPL.Syntax

lowerU :: Precision -> Stmt -> UQPLStmt

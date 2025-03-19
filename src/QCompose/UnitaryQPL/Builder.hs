module QCompose.UnitaryQPL.Builder where

import Control.Monad.State
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

type Builder a = State (Program a)

-- _oracleDecl :: Lens' (Program a) (OracleDecl a)
-- _oracleDecl f p@Program{proc_defs} = (\x -> p{proc_defs = x}) <$> f proc_defs

_procDefs :: Lens' (Program a) [ProcDef a]
_procDefs f p@Program{proc_defs} = (\x -> p{proc_defs = x}) <$> f proc_defs

_stmt :: Lens' (Program a) (Stmt a)
_stmt f p@Program{stmt} = (\x -> p{stmt = x}) <$> f stmt

addStmt :: Stmt a -> Builder a ()
addStmt s = _stmt %= SeqS . (: [s])

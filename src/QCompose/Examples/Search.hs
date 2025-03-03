module QCompose.Examples.Search where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

arraySearch :: SizeT -> Program SizeT
arraySearch n = Program{funCtx = FunCtx{oracle, funDefs = [check]}, stmt}
  where
    oracle = OracleDecl{paramTypes = [Fin n], retTypes = [Fin 2]}

    check :: FunDef SizeT
    check =
      FunDef
        { fun_name = "check"
        , params = [("i", Fin n)]
        , body =
            FunCallS
              { fun_kind = OracleCall
              , rets = ["b"]
              , args = ["i"]
              }
        , rets = [("b", Fin 2)]
        }

    stmt =
      FunCallS
        { fun_kind = SubroutineCall Contains
        , args = ["check"]
        , rets = ["result"]
        }

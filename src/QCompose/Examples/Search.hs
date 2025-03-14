module QCompose.Examples.Search where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

arraySearch :: SizeT -> Program SizeT
arraySearch n = Program{funCtx = FunCtx{oracle_decl, fun_defs = [check]}, stmt}
  where
    oracle_decl = OracleDecl{param_types = [Fin n], ret_types = [Fin 2]}

    check :: FunDef SizeT
    check =
      FunDef
        { fun_name = "check"
        , param_binds = [("i", Fin n)]
        , body =
            FunCallS
              { fun_kind = OracleCall
              , rets = ["b"]
              , args = ["i"]
              }
        , ret_binds = [("b", Fin 2)]
        }

    stmt =
      FunCallS
        { fun_kind = SubroutineCall Contains
        , args = ["check"]
        , rets = ["result"]
        }

arraySearchIx :: SizeT -> Program SizeT
arraySearchIx n = Program{funCtx = FunCtx{oracle_decl, fun_defs = [check]}, stmt}
  where
    oracle_decl = OracleDecl{param_types = [Fin n], ret_types = [Fin 2]}

    check :: FunDef SizeT
    check =
      FunDef
        { fun_name = "check"
        , param_binds = [("i", Fin n)]
        , body =
            FunCallS
              { fun_kind = OracleCall
              , rets = ["b"]
              , args = ["i"]
              }
        , ret_binds = [("b", Fin 2)]
        }

    stmt =
      FunCallS
        { fun_kind = SubroutineCall Search
        , args = ["check"]
        , rets = ["result", "solution"]
        }

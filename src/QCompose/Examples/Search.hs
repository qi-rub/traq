module QCompose.Examples.Search where

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang.Syntax

arraySearch :: SizeT -> Program SizeT
arraySearch n =
  Program
    { funCtx = FunCtx{oracle_decl, fun_defs = Ctx.fromList [("check", check)]}
    , stmt
    }
 where
  oracle_decl = OracleDecl{param_types = [Fin n], ret_types = [Fin 2]}

  check :: FunDef SizeT
  check =
    FunDef
      { fun_name = "check"
      , param_binds = [("i", Fin n)]
      , body =
          ExprS
            { rets = ["b"]
            , expr =
                FunCallE
                  { fun_kind = OracleCall
                  , args = ["i"]
                  }
            }
      , ret_binds = [("b", Fin 2)]
      }

  stmt =
    ExprS
      { expr =
          FunCallE
            { fun_kind = PrimitiveCall "any" ["check"]
            , args = []
            }
      , rets = ["result"]
      }

arraySearchIx :: SizeT -> Program SizeT
arraySearchIx n =
  Program
    { funCtx = FunCtx{oracle_decl, fun_defs = Ctx.fromList [("check", check)]}
    , stmt
    }
 where
  oracle_decl = OracleDecl{param_types = [Fin n], ret_types = [Fin 2]}

  check :: FunDef SizeT
  check =
    FunDef
      { fun_name = "check"
      , param_binds = [("i", Fin n)]
      , body =
          ExprS
            { rets = ["b"]
            , expr =
                FunCallE
                  { fun_kind = OracleCall
                  , args = ["i"]
                  }
            }
      , ret_binds = [("b", Fin 2)]
      }

  stmt =
    ExprS
      { rets = ["result", "solution"]
      , expr =
          FunCallE
            { fun_kind = PrimitiveCall "search" ["check"]
            , args = []
            }
      }

module QCompose.Examples.Search where

import Data.Void (Void)
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.ProtoLang.Syntax

arraySearch :: SizeT -> Program Void SizeT
arraySearch n =
  Program
    { funCtx = Ctx.fromListWith fun_name [oracle_decl]
    , stmt
    }
 where
  oracle_decl :: FunDef Void SizeT
  oracle_decl = FunDef{fun_name = "Oracle", param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  stmt =
    ExprS
      { expr =
          FunCallE
            { fun_kind = PrimitiveCall "any" ["Oracle"]
            , args = []
            }
      , rets = ["result"]
      }

arraySearchIx :: SizeT -> Program Void SizeT
arraySearchIx n =
  Program
    { funCtx = Ctx.fromListWith fun_name [oracle_decl, check]
    , stmt
    }
 where
  oracle_decl :: FunDef Void SizeT
  oracle_decl = FunDef{fun_name = "Oracle", param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  check :: FunDef Void SizeT
  check =
    FunDef
      { fun_name = "check"
      , param_types = [Fin n]
      , mbody =
          Just
            FunBody
              { param_names = ["i"]
              , body_stmt =
                  ExprS
                    { rets = ["b"]
                    , expr =
                        FunCallE
                          { fun_kind = FunctionCall "Oracle"
                          , args = ["i"]
                          }
                    }
              , ret_names = ["b"]
              }
      , ret_types = [Fin 2]
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

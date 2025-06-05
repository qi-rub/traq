module QCompose.Examples.Search where

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Primitives
import QCompose.Primitives.Search.Prelude (mkAny)
import QCompose.ProtoLang.Syntax

arraySearch :: SizeT -> Program DefaultPrims SizeT
arraySearch n =
  Program
    { funCtx = Ctx.fromListWith fun_name [oracle_decl]
    , stmt
    }
 where
  oracle_decl :: FunDef DefaultPrims SizeT
  oracle_decl = FunDef{fun_name = "Oracle", param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  stmt =
    ExprS
      { expr =
          FunCallE
            { fun_kind = PrimitiveCall $ QAny (mkAny "Oracle")
            , args = []
            }
      , rets = ["result"]
      }

arraySearchIx :: SizeT -> Program DefaultPrims SizeT
arraySearchIx n =
  Program
    { funCtx = Ctx.fromListWith fun_name [oracle_decl, check]
    , stmt
    }
 where
  oracle_decl :: FunDef DefaultPrims SizeT
  oracle_decl = FunDef{fun_name = "Oracle", param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  check :: FunDef DefaultPrims SizeT
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
            { fun_kind = PrimitiveCall (QAny $ QSearchCFNW{predicate = "check", return_sol = True})
            , args = []
            }
      }

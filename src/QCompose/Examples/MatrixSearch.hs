module QCompose.Examples.MatrixSearch where

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Primitives
import QCompose.ProtoLang.Syntax

matrixExample :: forall sizeT. sizeT -> sizeT -> VarType sizeT -> Program DefaultPrims sizeT
matrixExample n m tyBool =
  Program
    { funCtx = Ctx.fromListWith fun_name [oracle_decl, check_entry, check_row, check_matrix]
    , stmt =
        ExprS
          { expr = FunCallE{fun_kind = FunctionCall "HasAllOnesRow", args = []}
          , rets = ["result"]
          }
    }
 where
  tyI, tyJ :: VarType sizeT
  tyI = Fin n
  tyJ = Fin m

  oracle_decl :: FunDef DefaultPrims sizeT
  oracle_decl = FunDef{fun_name = "Oracle", param_types = [tyI, tyJ], ret_types = [tyBool], mbody = Nothing}

  check_entry :: FunDef DefaultPrims sizeT
  check_entry =
    FunDef
      { fun_name = "IsEntryZero"
      , param_types = [tyI, tyJ]
      , mbody =
          Just
            FunBody
              { param_names = [i, j]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [e], expr = FunCallE{fun_kind = FunctionCall "Oracle", args = [i, j]}}
                    , ExprS{rets = [e'], expr = UnOpE{un_op = NotOp, arg = e}}
                    ]
              , ret_names = [e']
              }
      , ret_types = [tyBool]
      }
   where
    i = "i0"
    j = "j0"
    e = "e"
    e' = "e'"

  check_row :: FunDef DefaultPrims sizeT
  check_row =
    FunDef
      { fun_name = "IsRowAllOnes"
      , param_types = [tyI]
      , mbody =
          Just
            FunBody
              { param_names = [i]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [ok], expr = FunCallE{fun_kind = PrimitiveCall (QAny $ mkAny "IsEntryZero"), args = [i]}}
                    , ExprS{rets = [ok'], expr = UnOpE{un_op = NotOp, arg = ok}}
                    ]
              , ret_names = [ok']
              }
      , ret_types = [tyBool]
      }
   where
    i = "i"
    ok = "okr"
    ok' = "okr'"

  check_matrix :: FunDef DefaultPrims sizeT
  check_matrix =
    FunDef
      { fun_name = "HasAllOnesRow"
      , param_types = []
      , mbody =
          Just
            FunBody
              { param_names = []
              , body_stmt = ExprS{rets = [ok], expr = FunCallE{fun_kind = PrimitiveCall (QAny $ mkAny "IsRowAllOnes"), args = []}}
              , ret_names = [ok]
              }
      , ret_types = [tyBool]
      }
   where
    ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program DefaultPrims SizeT
matrixExampleS n m = matrixExample n m (Fin 2)

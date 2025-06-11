module QCompose.Examples.MatrixSearch where

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.Primitives
import QCompose.Primitives.Search.Prelude (HasPrimAny (..))
import QCompose.ProtoLang.Syntax

matrixExample :: forall primsT sizeT. (HasPrimAny primsT) => sizeT -> sizeT -> VarType sizeT -> Program primsT sizeT
matrixExample n m tyBool =
  Program
    { funCtx = Ctx.fromList [(oracle_name, oracle_decl), (check_entry_name, check_entry), (check_row_name, check_row), (check_matrix_name, check_matrix)]
    , stmt =
        ExprS
          { expr = FunCallE{fun_kind = FunctionCall check_matrix_name, args = []}
          , rets = ["result"]
          }
    }
 where
  tyI, tyJ :: VarType sizeT
  tyI = Fin n
  tyJ = Fin m

  oracle_name :: Ident
  oracle_name = "Oracle"

  oracle_decl :: FunDef primsT sizeT
  oracle_decl = FunDef{param_types = [tyI, tyJ], ret_types = [tyBool], mbody = Nothing}

  check_entry_name :: Ident
  check_entry_name = "IsEntryZero"

  check_entry :: FunDef primsT sizeT
  check_entry =
    FunDef
      { param_types = [tyI, tyJ]
      , mbody =
          Just
            FunBody
              { param_names = [i, j]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [e], expr = FunCallE{fun_kind = FunctionCall oracle_name, args = [i, j]}}
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

  check_row_name :: Ident
  check_row_name = "IsRowAllOnes"

  check_row :: FunDef primsT sizeT
  check_row =
    FunDef
      { param_types = [tyI]
      , mbody =
          Just
            FunBody
              { param_names = [i]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [ok], expr = FunCallE{fun_kind = PrimitiveCall (mkAny check_entry_name), args = [i]}}
                    , ExprS{rets = [ok'], expr = UnOpE{un_op = NotOp, arg = ok}}
                    ]
              , ret_names = [ok']
              }
      , ret_types = [tyBool]
      }
   where
    i = "i"
    ok = "hasZero"
    ok' = "okr"

  check_matrix_name :: Ident
  check_matrix_name = "HasAllOnesRow"

  check_matrix :: FunDef primsT sizeT
  check_matrix =
    FunDef
      { param_types = []
      , mbody =
          Just
            FunBody
              { param_names = []
              , body_stmt = ExprS{rets = [ok], expr = FunCallE{fun_kind = PrimitiveCall (mkAny check_row_name), args = []}}
              , ret_names = [ok]
              }
      , ret_types = [tyBool]
      }
   where
    ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program DefaultPrims SizeT
matrixExampleS n m = matrixExample n m (Fin 2)

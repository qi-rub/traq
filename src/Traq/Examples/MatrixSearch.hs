module Traq.Examples.MatrixSearch where

import Data.String (fromString)

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.Prelude (HasPrimAny (..))
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck (tbool)

matrixExample :: forall primsT sizeT. (HasPrimAny primsT, Num sizeT) => sizeT -> sizeT -> Program primsT sizeT
matrixExample n m =
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
  oracle_name = "Matrix"

  oracle_decl :: FunDef primsT sizeT
  oracle_decl = FunDef{param_types = [tyI, tyJ], ret_types = [tbool], mbody = Nothing}

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
                    , ExprS{rets = [e'], expr = BasicExprE UnOpE{un_op = NotOp, operand = fromString e}}
                    ]
              , ret_names = [e']
              }
      , ret_types = [tbool]
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
                    , ExprS{rets = [ok'], expr = BasicExprE UnOpE{un_op = NotOp, operand = fromString ok}}
                    ]
              , ret_names = [ok']
              }
      , ret_types = [tbool]
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
      , ret_types = [tbool]
      }
   where
    ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program DefaultPrims SizeT
matrixExampleS = matrixExample

module Traq.Examples.MatrixSearch where

import Data.String (fromString)

import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.Prelude
import Traq.ProtoLang.Syntax
import Traq.ProtoLang.TypeCheck (tbool)

mkMatrixExample ::
  forall ext sizeT.
  (sizeT ~ SizeType ext, Num sizeT) =>
  (VarType sizeT -> PartialFun -> Expr ext) ->
  sizeT ->
  sizeT ->
  Program ext
mkMatrixExample mkAny n m =
  Program
    [ NamedFunDef oracle_name oracle_decl
    , NamedFunDef check_entry_name check_entry
    , NamedFunDef check_row_name check_row
    , NamedFunDef check_matrix_name check_matrix
    ]
 where
  tyI, tyJ :: VarType sizeT
  tyI = Fin n
  tyJ = Fin m

  oracle_name :: Ident
  oracle_name = "Matrix"

  oracle_decl :: FunDef ext
  oracle_decl = FunDef{param_types = [tyI, tyJ], ret_types = [tbool], mbody = Nothing}

  check_entry_name :: Ident
  check_entry_name = "IsEntryZero"

  check_entry :: FunDef ext
  check_entry =
    FunDef
      { param_types = [tyI, tyJ]
      , mbody =
          Just
            FunBody
              { param_names = [i, j]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [e], expr = FunCallE{fname = oracle_name, args = [i, j]}}
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

  check_row :: FunDef ext
  check_row =
    FunDef
      { param_types = [tyI]
      , mbody =
          Just
            FunBody
              { param_names = [i]
              , body_stmt =
                  SeqS
                    [ ExprS{rets = [ok], expr = mkAny tyJ (PartialFun check_entry_name [Just i, Nothing])}
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

  check_matrix :: FunDef ext
  check_matrix =
    FunDef
      { param_types = []
      , mbody =
          Just
            FunBody
              { param_names = []
              , body_stmt = ExprS{rets = [ok], expr = mkAny tyI (PartialFun check_row_name [Nothing])}
              , ret_names = [ok]
              }
      , ret_types = [tbool]
      }
   where
    ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program (DefaultPrims SizeT precT)
matrixExampleS = mkMatrixExample (\ty pfun -> PrimCallE $ Primitive [pfun] $ QAny $ QSearchCFNW $ PrimSearch AnyK ty)

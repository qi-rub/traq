{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.Examples.MatrixSearch where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

matrixExample :: forall a. a -> a -> VarType a -> Program a
matrixExample n m tyBool =
  Program
    { funCtx =
        FunCtx
          { oracle_decl = OracleDecl [tyI, tyJ] [tyBool]
          , fun_defs = [check_entry, check_row, check_matrix]
          }
    , stmt =
        ExprS
          { expr = FunCallE{fun_kind = FunctionCall "HasAllOnesRow", args = []}
          , rets = ["result"]
          }
    }
 where
  tyI, tyJ :: VarType a
  tyI = Fin n
  tyJ = Fin m

  -- check_entry :: FunDef a
  check_entry =
    FunDef
      "IsEntryZero"
      [(i, tyI), (j, tyJ)]
      [(e', tyBool)]
      ( SeqS
          [ ExprS{rets = [e], expr = FunCallE{fun_kind = OracleCall, args = [i, j]}}
          , ExprS{rets = [e'], expr = UnOpE{un_op = NotOp, arg = e}}
          ]
      )
   where
    i = "i0"
    j = "j0"
    e = "e"
    e' = "e'"

  -- check_row :: FunDef a
  check_row =
    FunDef
      "IsRowAllOnes"
      [(i, tyI)]
      [(ok', tyBool)]
      ( SeqS
          [ ExprS{rets = [ok], expr = FunCallE{fun_kind = SubroutineCall Contains, args = ["IsEntryZero", i]}}
          , ExprS{rets = [ok'], expr = UnOpE{un_op = NotOp, arg = ok}}
          ]
      )
   where
    i = "i"
    ok = "okr"
    ok' = "okr'"

  -- check_matrix :: FunDef a
  check_matrix =
    FunDef
      "HasAllOnesRow"
      []
      [(ok, tyBool)]
      ExprS{rets = [ok], expr = FunCallE{fun_kind = SubroutineCall Contains, args = ["IsRowAllOnes"]}}
   where
    ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program SizeT
matrixExampleS n m = matrixExample n m (Fin 2)

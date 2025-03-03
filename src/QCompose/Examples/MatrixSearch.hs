module QCompose.Examples.MatrixSearch where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

matrixExample :: a -> a -> VarType a -> Program a
matrixExample n m tyBool =
  Program
    { funCtx =
        FunCtx
          { oracle = OracleDecl [tyI, tyJ] [tyBool]
          , funDefs = [check_entry, check_row, check_matrix]
          }
    , stmt = FunCallS{fun_kind = FunctionCall "HasAllOnesRow", rets = ["result"], args = []}
    }
  where
    -- tyI, tyJ :: VarType a
    tyI = Fin n
    tyJ = Fin m

    -- check_entry :: FunDef a
    check_entry =
      FunDef
        "IsEntryZero"
        [(i, tyI), (j, tyJ)]
        [(e', tyBool)]
        ( SeqS
            [ FunCallS{fun_kind = OracleCall, rets = [e], args = [i, j]}
            , UnOpS{ret = e', un_op = NotOp, arg = e}
            ]
        )
      where
        i = "i"
        j = "j"
        e = "e"
        e' = "e'"

    -- check_row :: FunDef a
    check_row =
      FunDef
        "IsRowAllOnes"
        [(i, tyI)]
        [(ok', tyBool)]
        ( SeqS
            [ FunCallS{fun_kind = SubroutineCall Contains, rets = ["ok"], args = ["IsEntryZero", i]}
            , UnOpS{ret = ok', un_op = NotOp, arg = ok}
            ]
        )
      where
        i = "i"
        ok = "ok"
        ok' = "ok'"

    -- check_matrix :: FunDef a
    check_matrix =
      FunDef
        "HasAllOnesRow"
        []
        [(ok, tyBool)]
        (FunCallS{fun_kind = SubroutineCall Contains, rets = [ok], args = ["IsRowAllOnes"]})
      where
        ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program SizeT
matrixExampleS n m = matrixExample n m (Fin 2)

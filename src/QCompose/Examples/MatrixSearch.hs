module QCompose.Examples.MatrixSearch where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

matrixExample :: SizeT -> SizeT -> Program SizeT
matrixExample n m =
  Program
    { funCtx =
        FunCtx
          { oracle = OracleDef [tyI, tyJ] [tyBool]
          , funDefs = [check_entry, check_row, check_matrix]
          }
    , stmt = SFunCall{rets = ["result"], fun = "check_matrix", args = []}
    }
  where
    tyBool, tyI, tyJ :: VarType SizeT
    tyBool = Fin 2
    tyI = Fin n
    tyJ = Fin m

    check_entry :: FunDef SizeT
    check_entry =
      FunDef
        "check_entry"
        [(i, tyI), (j, tyJ)]
        [(e', tyBool)]
        ( SSeq
            [ SOracle [e] [i, j]
            , SUnOp e' PNot e
            ]
        )
      where
        i = "i"
        j = "j"
        e = "e"
        e' = "e'"

    check_row :: FunDef SizeT
    check_row =
      FunDef
        "check_row"
        [(i, tyI)]
        [(ok', tyBool)]
        ( SSeq
            [ SContains ok "check_entry" [i]
            , SUnOp ok' PNot ok
            ]
        )
      where
        i = "i"
        ok = "ok"
        ok' = "ok'"

    check_matrix :: FunDef SizeT
    check_matrix =
      FunDef
        "check_matrix"
        []
        [(ok, tyBool)]
        (SContains ok "check_row" [])
      where
        ok = "ok"

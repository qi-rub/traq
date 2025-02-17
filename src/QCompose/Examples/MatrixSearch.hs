module QCompose.Examples.MatrixSearch where

import QCompose.Basic
import QCompose.ProtoLang.Syntax

sbool :: VarType
sbool = Fin (Right 2)

matrixExample :: SizeT -> SizeT -> Program
matrixExample n m =
  Program
    { funCtx =
        FunCtx
          { oracle = OracleDef [tyI, tyJ] [sbool]
          , funs = [check_entry, check_row, check_matrix]
          }
    , stmt = SFunCall{rets = ["result"], fun = "check_matrix", args = []}
    }
  where
    tyI, tyJ :: VarType
    tyI = Fin (Right n)
    tyJ = Fin (Right m)

    check_entry :: FunDef
    check_entry =
      FunDef
        "check_entry"
        [(i, tyI), (j, tyJ)]
        [(e', sbool)]
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

    check_row :: FunDef
    check_row =
      FunDef
        "check_row"
        [(i, tyI)]
        [(ok', sbool)]
        ( SSeq
            [ SContains ok "check_entry" [i]
            , SUnOp ok' PNot ok
            ]
        )
      where
        i = "i"
        ok = "ok"
        ok' = "ok'"

    check_matrix :: FunDef
    check_matrix =
      FunDef
        "check_matrix"
        []
        [(ok, sbool)]
        (SContains ok "check_row" [])
      where
        ok = "ok"

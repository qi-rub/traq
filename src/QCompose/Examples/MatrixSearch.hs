module QCompose.Examples.MatrixSearch where

import qualified Data.Map as M
import QCompose.Basic
import QCompose.ProtoLang.Syntax

sbool :: VarType
sbool = Fin (Right 2)

matrixExample :: SizeT -> SizeT -> FunCtx
matrixExample n m =
  FunCtx
    { oracle = OracleDef [tyI, tyJ] [sbool]
    , funs =
        M.fromList
          [ ("check_entry", check_entry)
          , ("check_row", check_row)
          , ("check_matrix", check_matrix)
          ]
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
            (SOracle [e] [i, j])
            (SUnOp e' PNot e)
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
            (SContains ok "check_entry" [i])
            (SUnOp ok' PNot ok)
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

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
    , stmt = FunCallS{rets = ["result"], fun = "check_matrix", args = []}
    }
  where
    -- tyI, tyJ :: VarType a
    tyI = Fin n
    tyJ = Fin m

    -- check_entry :: FunDef a
    check_entry =
      FunDef
        "check_entry"
        [(i, tyI), (j, tyJ)]
        [(e', tyBool)]
        ( SeqS
            [ OracleS [e] [i, j]
            , UnOpS e' NotOp e
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
        "check_row"
        [(i, tyI)]
        [(ok', tyBool)]
        ( SeqS
            [ ContainsS ok "check_entry" [i]
            , UnOpS ok' NotOp ok
            ]
        )
      where
        i = "i"
        ok = "ok"
        ok' = "ok'"

    -- check_matrix :: FunDef a
    check_matrix =
      FunDef
        "check_matrix"
        []
        [(ok, tyBool)]
        (ContainsS ok "check_row" [])
      where
        ok = "ok"

matrixExampleS :: SizeT -> SizeT -> Program SizeT
matrixExampleS n m = matrixExample n m (Fin 2)

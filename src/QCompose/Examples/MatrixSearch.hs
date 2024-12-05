module QCompose.Examples.MatrixSearch where

import qualified Data.Map as M
import QCompose.ProtoLang

sbool :: VarType
sbool = Fin 2

matrixExample :: Int -> Int -> FunCtx
matrixExample n m =
  M.fromList
    [ ("check_entry", check_entry),
      ("check_row", check_row),
      ("check_matrix", check_matrix)
    ]
  where
    check_entry :: FunDef
    check_entry =
      FunDef
        [(i, Fin n), (j, Fin m)]
        [(e', sbool)]
        $ SSeq
          (SOracle [e] [i, j])
          (SUnOp e' PNot e)
      where
        i = "i"
        j = "j"
        e = "e"
        e' = "e'"

    check_row :: FunDef
    check_row =
      FunDef
        [(i, Fin n)]
        [(ok', sbool)]
        $ SSeq
          (SContains ok "check_entry" [i])
          (SUnOp ok' PNot ok)
      where
        i = "i"
        ok = "ok"
        ok' = "ok'"

    check_matrix :: FunDef
    check_matrix =
      FunDef
        []
        [(ok, sbool)]
        (SContains ok "check_row" [])
      where
        ok = "ok"

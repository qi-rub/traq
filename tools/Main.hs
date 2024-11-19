{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Quipper (QData, print_generic, Circ, Qubit, Format(PDF), named_gate, gate_Z, qubit, qinit)
import Quipper.Internal.Monad (qinit_list)
import Quipper.Libraries.Qureg (Qureg, qureg_shape)
import QCompose.Basic
import QCompose.Amplify
import QCompose.ProtoLang
import Text.Pretty.Simple
import qualified Data.Map as M

-- Example 1. Matrix Search
matrix_example :: Int -> Int -> FunCtx
matrix_example n m =
  M.fromList 
  [ ("check_entry", check_entry)
  , ("check_row", check_row)
  , ("check_matrix", check_matrix)
  ]
  where
    check_entry :: FunDef
    check_entry = 
      FunDef
        (PFunType [Fin n, Fin m] [pbool])
        [i, j] $
        SLet [e] (E $ EOracle [i, j]) $
        SLet [e'] (E $ EUnOp PNot e) $
        SReturn [e']
      where
        i = "i"
        j = "j"
        e = "e"
        e' = "e'"

    check_row :: FunDef
    check_row = 
      FunDef
        (PFunType [Fin n] [pbool])
        [i] $
        SLet [c, ok] (ESearch "check_entry" [i]) $
        SLet [ok'] (E $ EUnOp PNot ok) $
        SReturn [ok']
      where
        i = "i"
        c = "c"
        ok = "ok"
        ok' = "ok'"

    check_matrix :: FunDef
    check_matrix =
      FunDef
        (PFunType [] [])
        [] $
        SLet [r, ok] (ESearch "check_row" []) $
        SReturn [ok]
        where
          r = "r"
          ok = "ok"


unif :: QData qa => Unitary qa
unif = named_gate "Uniform"

oracle :: QData qa => qa -> Circ (qa, [Qubit], Qubit)
oracle q = do
  w <- qinit_list [1, 1, 1]
  res <- qinit 0
  named_gate "Oracle" (q, w, res)
  return (q, w, res)

main :: IO ()
main = do
  let ex = matrix_example 2 3
  pPrint $ ex
  let (FunDef _ _ body) = (ex M.! "check_matrix")
  pPrint $ flatten_seq $ compile_classical (M.empty) body
  -- print_generic PDF (amplify 2 unif oracle) qs
  -- where
  --   qs = qureg_shape 3



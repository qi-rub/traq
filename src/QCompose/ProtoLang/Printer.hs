{-# LANGUAGE ScopedTypeVariables #-}

module QCompose.ProtoLang.Printer where

import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Printing

instance (Show a) => ToCodeString (VarType a) where
  toCodeString (Fin len) = "Fin<" <> show len <> ">"

instance ToCodeString UnOp where
  toCodeString NotOp = "!"

instance ToCodeString BinOp where
  toCodeString AddOp = "+"
  toCodeString LEqOp = "<="
  toCodeString AndOp = "/\\"

instance (Show a) => ToCodeString (Stmt a) where
  toCodeLines AssignS{..} = [unwords [ret, "<-", arg]]
  toCodeLines ConstS{..} = [unwords [ret, "<-", show val, ":", toCodeString ty]]
  toCodeLines UnOpS{..} = [unwords [ret, "<-", toCodeString un_op <> arg]]
  toCodeLines BinOpS{..} = [unwords [ret, "<-", lhs, toCodeString bin_op, rhs]]
  toCodeLines OracleS{..} =
    [ unwords
        [ commaList rets
        , "<-"
        , "Oracle(" <> commaList args <> ")"
        ]
    ]
  toCodeLines FunCallS{..} =
    [ unwords
        [ commaList rets
        , "<-"
        , fun <> "(" <> commaList args <> ")"
        ]
    ]
  toCodeLines IfThenElseS{..} =
    [unwords ["if", cond, "then"]]
      <> indent (toCodeLines s_true)
      <> ["else"]
      <> indent (toCodeLines s_false)
      <> ["end"]
  toCodeLines (SeqS ss) = concatMap toCodeLines ss
  toCodeLines SearchS{..} =
    [ unwords
        [ commaList [sol, ok]
        , "<-"
        , "search" <> "(" <> commaList (predicate : args) <> ")"
        ]
    ]
  toCodeLines ContainsS{..} =
    [ unwords
        [ ok
        , "<-"
        , "any" <> "(" <> commaList (predicate : args) <> ")"
        ]
    ]

instance (Show a) => ToCodeString (FunDef a) where
  toCodeLines FunDef{..} =
    [unwords ["def", name, "(" <> commaList (showTypedVar <$> params) <> ")", "do"]]
      <> indent
        ( toCodeLines body
            <> [unwords ["return", commaList (showTypedVar <$> rets)]]
        )
      <> ["end"]
    where
      showTypedVar :: (Ident, VarType a) -> String
      showTypedVar (x, ty) = unwords [x, ":", toCodeString ty]

instance (Show a) => ToCodeString (OracleDef a) where
  toCodeString OracleDef{..} =
    unwords
      [ "declare"
      , "Oracle" <> "(" <> commaList (toCodeString <$> paramTypes) <> ")"
      , "->"
      , commaList (toCodeString <$> retTypes)
      ]

instance (Show a) => ToCodeString (Program a) where
  toCodeLines Program{funCtx = FunCtx{..}, ..} =
    [toCodeString oracle, ""]
      <> fs
      <> toCodeLines stmt
    where
      fs = map toCodeString funDefs

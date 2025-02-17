module QCompose.ProtoLang.Printer where

import Data.List (intercalate)
import QCompose.Basic
import QCompose.ProtoLang.Syntax

class ToCodeString a where
  toCodeString :: a -> String
  toCodeString = unlines . toCodeLines

  toCodeLines :: a -> [String]
  toCodeLines = pure . toCodeString

instance ToCodeString VarType where
  toCodeString (Fin len) = "Fin<" <> len' <> ">"
    where
      len' = case len of
        (Left s) -> s
        Right n -> show n

instance ToCodeString UnOp where
  toCodeString PNot = "!"

instance ToCodeString BinOp where
  toCodeString PAdd = "+"
  toCodeString PLeq = "<="
  toCodeString PAnd = "/\\"

commaList :: [String] -> String
commaList = intercalate ", "

indent :: [String] -> [String]
indent = map ("  " <>)

instance ToCodeString Stmt where
  toCodeLines SAssign{..} = [unwords [ret, "<-", arg]]
  toCodeLines SConst{..} = [unwords [ret, "<-", show val, ":", toCodeString ty]]
  toCodeLines SUnOp{..} = [unwords [ret, "<-", toCodeString un_op <> arg]]
  toCodeLines SBinOp{..} = [unwords [ret, "<-", lhs, toCodeString bin_op, rhs]]
  toCodeLines SOracle{..} =
    [ unwords
        [ commaList rets
        , "<-"
        , "Oracle(" <> commaList args <> ")"
        ]
    ]
  toCodeLines SFunCall{..} =
    [ unwords
        [ commaList rets
        , "<-"
        , fun <> "(" <> commaList args <> ")"
        ]
    ]
  toCodeLines SIfTE{..} =
    [unwords ["if", cond, "then"]]
      <> indent (toCodeLines s_true)
      <> ["else"]
      <> indent (toCodeLines s_false)
      <> ["end"]
  toCodeLines (SSeq ss) = concatMap toCodeLines ss
  toCodeLines SSearch{..} =
    [ unwords
        [ commaList [sol, ok]
        , "<-"
        , "search" <> "(" <> commaList (predicate : args) <> ")"
        ]
    ]
  toCodeLines SContains{..} =
    [ unwords
        [ ok
        , "<-"
        , "any" <> "(" <> commaList (predicate : args) <> ")"
        ]
    ]

instance ToCodeString FunDef where
  toCodeLines FunDef{..} =
    [unwords ["def", name, "(" <> commaList (showTypedVar <$> params) <> ")", "do"]]
      <> indent
        ( toCodeLines body
            <> [unwords ["return", commaList (showTypedVar <$> rets)]]
        )
      <> ["end"]
    where
      showTypedVar :: (Ident, VarType) -> String
      showTypedVar (x, ty) = unwords [x, ":", toCodeString ty]

instance ToCodeString OracleDef where
  toCodeString OracleDef{..} =
    unwords
      [ "declare"
      , "Oracle" <> "(" <> commaList (toCodeString <$> paramTypes) <> ")"
      , "->"
      , commaList (toCodeString <$> retTypes)
      ]

instance ToCodeString Program where
  toCodeLines Program{funCtx = FunCtx{..}, ..} =
    [toCodeString oracle, ""]
      <> fs
      <> toCodeLines stmt
    where
      fs = toCodeString <$> funs

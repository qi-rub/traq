module QCompose.ProtoLang.Parser where

import Control.Monad
import QCompose.Basic
import QCompose.ProtoLang.Syntax
import Text.Parsec (ParseError, choice, eof, many, parse, (<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (
  GenTokenParser (
    TokenParser,
    angles,
    commaSep1,
    identifier,
    integer,
    operator,
    reserved,
    reservedOp,
    semiSep,
    symbol
  ),
  TokenParser,
  commentLine,
  makeTokenParser,
  reservedNames,
  reservedOpNames,
 )

protoLangDef :: LanguageDef st
protoLangDef =
  emptyDef
    { commentLine = "//"
    , reservedNames =
        [ -- statements
          "Oracle"
        , "const"
        , -- functions
          "def"
        , "do"
        , "return"
        , "end"
        , "declare"
        , -- types
          "Fin"
        , "Bool"
        , -- primitives
          "search"
        , "any"
        ]
    , reservedOpNames = [":", "<-"]
    }

protoLangTokenParser :: TokenParser st
protoLangTokenParser = makeTokenParser protoLangDef

parseStmt :: Parser Stmt
parseStmt = parseSeq
  where
    TokenParser{..} = protoLangTokenParser

    parseSeq = SSeq <$> semiSep parseSingleStmt

    parseType :: Parser VarType
    parseType = parseBool <|> parseFin
      where
        parseBool = reserved "Bool" >> return (Fin (Right 2))
        parseFin = do
          reserved "Fin"
          n <- angles integer
          return $ Fin (Right $ fromIntegral n)

    parseTypedExpr :: Parser a -> Parser (a, VarType)
    parseTypedExpr pa = (,) <$> pa <*> (reservedOp ":" >> parseType)

    parseSingleStmt :: Parser Stmt
    parseSingleStmt = do
      rets <- commaSep1 identifier
      reservedOp "<-"
      choice
        [ p rets
        | p <-
            [ checkSingleRet >=> parseAssign
            , checkSingleRet >=> parseConst
            , checkSingleRet >=> parseUnOpS
            ]
        ]

    checkSingleRet :: [Ident] -> Parser Ident
    checkSingleRet [ret] = return ret
    checkSingleRet _ = fail "expected one return value"

    parseAssign :: Ident -> Parser Stmt
    parseAssign ret = do
      arg <- identifier
      return SAssign{..}

    parseConst :: Ident -> Parser Stmt
    parseConst ret = do
      reserved "const"
      (val, ty) <- parseTypedExpr integer
      return SConst{..}

    parseUnOp :: Parser UnOp
    parseUnOp =
      operator >>= \case
        "!" -> return PNot
        _ -> fail "invalid unary operator"

    parseUnOpS :: Ident -> Parser Stmt
    parseUnOpS ret = do
      un_op <- parseUnOp
      arg <- identifier
      return SUnOp{..}

parseFunDef :: Parser FunDef
parseFunDef = undefined

parseOracleDef :: Parser OracleDef
parseOracleDef = undefined

parseProgram :: Parser Program
parseProgram = do
  oracle <- parseOracleDef
  funs <- many parseFunDef
  body <- parseStmt
  return Program{funCtx = FunCtx{oracle, funs}, body}

parseCode :: String -> Either ParseError Stmt
parseCode = parse (parseStmt <* eof) ""

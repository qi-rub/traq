module QCompose.ProtoLang.Parser where

import Control.Monad
import Data.Functor (($>))
import QCompose.Basic
import QCompose.ProtoLang.Syntax
import Text.Parsec (ParseError, choice, eof, many, optional, parse, try, (<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (
  GenTokenParser (
    TokenParser,
    angles,
    commaSep,
    commaSep1,
    identifier,
    integer,
    operator,
    parens,
    reserved,
    reservedOp,
    semi,
    semiSep,
    symbol,
    whiteSpace
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
    , reservedOpNames = [":", "<-", "->"]
    }

protoLangTokenParser :: TokenParser st
protoLangTokenParser = makeTokenParser protoLangDef

varType :: TokenParser () -> Parser VarType
varType TokenParser{..} = parseBool <|> parseFin
  where
    parseBool = reserved "Bool" $> Fin (Right 2)
    parseFin = do
      reserved "Fin"
      n <- angles ((Right <$> integer) <|> (Left <$> identifier))
      return $ Fin (fromIntegral <$> n)

typedExpr :: TokenParser () -> Parser a -> Parser (a, VarType)
typedExpr tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

stmt :: TokenParser () -> Parser Stmt
stmt tp@TokenParser{..} = seqS
  where
    seqS = SSeq <$> many (try (singleStmt <* optional semi))

    singleStmt :: Parser Stmt
    singleStmt = do
      rets <- commaSep1 identifier
      reservedOp "<-"
      choice
        [ try $ p rets
        | p <-
            [ oracleS
            , guardSingleRet >=> anyS
            , funCallS
            , guardSingleRet >=> assignS
            , guardSingleRet >=> constS
            , guardSingleRet >=> unOpS
            ]
        ]

    guardSingleRet :: [Ident] -> Parser Ident
    guardSingleRet [ret] = return ret
    guardSingleRet _ = fail "expected exactly one return value"

    assignS :: Ident -> Parser Stmt
    assignS ret = do
      arg <- identifier
      return SAssign{..}

    constS :: Ident -> Parser Stmt
    constS ret = do
      reserved "const"
      (val, ty) <- typedExpr tp integer
      return SConst{..}

    unOp :: Parser UnOp
    unOp =
      operator >>= \case
        "!" -> return PNot
        _ -> fail "invalid unary operator"

    unOpS :: Ident -> Parser Stmt
    unOpS ret = do
      un_op <- unOp
      arg <- identifier
      return SUnOp{..}

    oracleS :: [Ident] -> Parser Stmt
    oracleS rets = do
      reserved "Oracle"
      args <- parens $ commaSep identifier
      return SOracle{..}

    anyS :: Ident -> Parser Stmt
    anyS ok = do
      reserved "any"
      (predicate : args) <- parens $ commaSep1 identifier
      return SContains{..}

    funCallS :: [Ident] -> Parser Stmt
    funCallS rets = do
      fun <- identifier
      args <- parens $ commaSep identifier
      return SFunCall{..}

funDef :: TokenParser () -> Parser FunDef
funDef tp@TokenParser{..} = do
  reserved "def"
  name <- identifier
  params <- parens $ commaSep $ typedExpr tp identifier
  reserved "do"
  body <- stmt tp
  reserved "return"
  rets <- commaSep $ typedExpr tp identifier
  reserved "end"
  return FunDef{..}

oracleDef :: TokenParser () -> Parser OracleDef
oracleDef tp@TokenParser{..} = do
  reserved "declare"
  reserved "Oracle"
  paramTypes <- parens (commaSep (varType tp))
  reservedOp "->"
  retTypes <- commaSep (varType tp)
  return OracleDef{..}

program :: TokenParser () -> Parser Program
program tp@TokenParser{..} = do
  whiteSpace
  oracle <- oracleDef tp
  funs <- many (funDef tp)
  body <- stmt tp
  eof
  return Program{funCtx = FunCtx{oracle, funs}, body}

parseCode :: String -> Either ParseError Stmt
parseCode = parse (stmt protoLangTokenParser <* eof) ""

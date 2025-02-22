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

symbSize :: TokenParser () -> Parser SymbSize
symbSize TokenParser{..} = (Value . fromIntegral <$> integer) <|> (SymExpr <$> identifier)

varType :: TokenParser () -> Parser (VarType SymbSize)
varType tp@TokenParser{..} = boolType <|> finType
  where
    boolType = reserved "Bool" $> Fin (Value 2)
    finType = reserved "Fin" >> Fin <$> angles (symbSize tp)

typedExpr :: TokenParser () -> Parser a -> Parser (a, VarType SymbSize)
typedExpr tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

stmtP :: TokenParser () -> Parser (Stmt SymbSize)
stmtP tp@TokenParser{..} = SeqS <$> many (try (singleStmt <* optional semi))
  where
    singleStmt :: Parser (Stmt SymbSize)
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
            , guardSingleRet >=> binOpS
            ]
        ]

    guardSingleRet :: [Ident] -> Parser Ident
    guardSingleRet [ret] = return ret
    guardSingleRet _ = fail "expected exactly one return value"

    assignS :: Ident -> Parser (Stmt SymbSize)
    assignS ret = do
      arg <- identifier
      return AssignS{..}

    constS :: Ident -> Parser (Stmt SymbSize)
    constS ret = do
      reserved "const"
      (val, ty) <- typedExpr tp integer
      return ConstS{..}

    unOp :: Parser UnOp
    unOp =
      operator >>= \case
        "!" -> return NotOp
        _ -> fail "invalid unary operator"

    unOpS :: Ident -> Parser (Stmt SymbSize)
    unOpS ret = do
      un_op <- unOp
      arg <- identifier
      return UnOpS{..}

    binOp :: Parser BinOp
    binOp =
      operator >>= \case
        "+" -> return AddOp
        "<=" -> return LEqOp
        "&&" -> return AndOp
        _ -> fail "invalid binary operator"

    binOpS :: Ident -> Parser (Stmt SymbSize)
    binOpS ret = do
      lhs <- identifier
      bin_op <- binOp
      rhs <- identifier
      return BinOpS{..}

    oracleS :: [Ident] -> Parser (Stmt SymbSize)
    oracleS rets = do
      reserved "Oracle"
      args <- parens $ commaSep identifier
      return OracleS{..}

    anyS :: Ident -> Parser (Stmt SymbSize)
    anyS ok = do
      reserved "any"
      (predicate : args) <- parens $ commaSep1 identifier
      return ContainsS{..}

    funCallS :: [Ident] -> Parser (Stmt SymbSize)
    funCallS rets = do
      fun <- identifier
      args <- parens $ commaSep identifier
      return FunCallS{..}

funDef :: TokenParser () -> Parser (FunDef SymbSize)
funDef tp@TokenParser{..} = do
  reserved "def"
  name <- identifier
  params <- parens $ commaSep $ typedExpr tp identifier
  reserved "do"
  body <- stmtP tp
  reserved "return"
  rets <- commaSep $ typedExpr tp identifier
  reserved "end"
  return FunDef{..}

oracleDecl :: TokenParser () -> Parser (OracleDecl SymbSize)
oracleDecl tp@TokenParser{..} = do
  reserved "declare"
  reserved "Oracle"
  paramTypes <- parens (commaSep (varType tp))
  reservedOp "->"
  retTypes <- commaSep (varType tp)
  return OracleDecl{..}

program :: TokenParser () -> Parser (Program SymbSize)
program tp@TokenParser{..} = do
  whiteSpace
  oracle <- oracleDecl tp
  funDefs <- many (funDef tp)
  stmt <- stmtP tp
  eof
  return Program{funCtx = FunCtx{..}, ..}

parseCode :: String -> Either ParseError (Stmt SymbSize)
parseCode = parse (stmtP protoLangTokenParser <* eof) ""

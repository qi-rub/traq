{-# LANGUAGE RecordWildCards #-}

module QCompose.ProtoLang.Parser where

import Control.Monad ((>=>))
import Data.Either (isRight)
import Data.Functor (($>))
import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.Utils.Printing
import Text.Parsec (ParseError, choice, eof, many, optional, parse, try, (<|>))
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (
  GenTokenParser (..),
  TokenParser,
  commentLine,
  makeTokenParser,
  reservedNames,
  reservedOpNames,
 )

-- TODO unify
subroutines :: [(Subroutine, Ident)]
subroutines = [(sub, toCodeString sub) | sub <- [minBound ..]]

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
        ]
          ++ map snd subroutines
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
            [ funCallS
            , guardSingleRet >=> assignS
            , guardSingleRet >=> constS
            , guardSingleRet >=> unOpS
            , guardSingleRet >=> binOpS
            ]
        ]

    funCallKind :: Parser FunctionCallKind
    funCallKind = oracleCall <|> subroutineCall <|> functionCall
      where
        oracleCall = reserved "Oracle" $> OracleCall
        subroutineCall =
          choice
            [ reserved sname $> SubroutineCall sub
            | (sub, sname) <- subroutines
            ]
        functionCall = FunctionCall <$> identifier

    funCallS :: [Ident] -> Parser (Stmt SymbSize)
    funCallS rets = do
      fun_kind <- funCallKind
      args <- parens $ commaSep identifier
      return FunCallS{fun_kind, rets, args}

    guardSingleRet :: [Ident] -> Parser Ident
    guardSingleRet [ret] = return ret
    guardSingleRet _ = fail "expected exactly one return value"

    assignS :: Ident -> Parser (Stmt SymbSize)
    assignS ret = do
      arg <- identifier
      return AssignS{ret, arg}

    constS :: Ident -> Parser (Stmt SymbSize)
    constS ret = do
      reserved "const"
      (val, ty) <- typedExpr tp integer
      return ConstS{ret, val, ty}

    unOp :: Parser UnOp
    unOp =
      operator >>= \case
        "!" -> return NotOp
        _ -> fail "invalid unary operator"

    unOpS :: Ident -> Parser (Stmt SymbSize)
    unOpS ret = do
      un_op <- unOp
      arg <- identifier
      return UnOpS{ret, un_op, arg}

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
      return BinOpS{ret, bin_op, lhs, rhs}

funDef :: TokenParser () -> Parser (FunDef SymbSize)
funDef tp@TokenParser{..} = do
  reserved "def"
  fun_name <- identifier
  param_binds <- parens $ commaSep $ typedExpr tp identifier
  reserved "do"
  body <- stmtP tp
  reserved "return"
  ret_binds <- commaSep $ typedExpr tp identifier
  reserved "end"
  return FunDef{fun_name, param_binds, ret_binds, body}

oracleDecl :: TokenParser () -> Parser (OracleDecl SymbSize)
oracleDecl tp@TokenParser{..} = do
  reserved "declare"
  reserved "Oracle"
  param_types <- parens (commaSep (varType tp))
  reservedOp "->"
  ret_types <- commaSep (varType tp)
  return OracleDecl{param_types, ret_types}

program :: TokenParser () -> Parser (Program SymbSize)
program tp@TokenParser{..} = do
  oracle_decl <- oracleDecl tp
  fun_defs <- many (funDef tp)
  stmt <- stmtP tp
  return Program{funCtx = FunCtx{oracle_decl, fun_defs}, stmt}

parseCode :: (TokenParser () -> Parser a) -> String -> Either ParseError a
parseCode parser = parse (whiteSpace p *> parser p <* eof) ""
  where
    p = protoLangTokenParser

parseProgram :: String -> Either ParseError (Program SymbSize)
parseProgram = parseCode program

parseFunDef :: String -> Either ParseError (FunDef SymbSize)
parseFunDef = parseCode funDef

parseStmt :: String -> Either ParseError (Stmt SymbSize)
parseStmt = parseCode stmtP

isValidIdentifier :: String -> Bool
isValidIdentifier = isRight . parse (identifier protoLangTokenParser) ""

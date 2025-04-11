{-# LANGUAGE RecordWildCards #-}

module QCompose.ProtoLang.Parser (
  programParser,
  parseCode,
  parseProgram,
  parseFunDef,
  parseStmt,
  isValidIdentifier,
) where

import Data.Either (isRight)
import Data.Functor (($>))
import qualified Data.Number.Symbolic as Sym
import Text.Parsec (ParseError, char, choice, eof, many, parse, try, (<|>))
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

import QCompose.Prelude
import QCompose.ProtoLang.Syntax

protoLangDef :: LanguageDef st
protoLangDef =
  emptyDef
    { commentLine = "//"
    , reservedNames =
        [ -- statements
          "const"
        , -- functions
          "def"
        , "declare"
        , "do"
        , "end"
        , "return"
        , -- types
          "Fin"
        , "Bool"
        ]
    , reservedOpNames = [":", "<-", "->"]
    }

protoLangTokenParser :: TokenParser st
protoLangTokenParser = makeTokenParser protoLangDef

symbSize :: TokenParser () -> Parser SymbSize
symbSize TokenParser{..} = (Sym.con . fromIntegral <$> integer) <|> (Sym.var <$> identifier)

varType :: TokenParser () -> Parser (VarType SymbSize)
varType tp@TokenParser{..} = boolType <|> finType
 where
  boolType = reserved "Bool" $> Fin (Sym.con 2)
  finType = reserved "Fin" >> Fin <$> angles (symbSize tp)

typedTerm :: TokenParser () -> Parser a -> Parser (a, VarType SymbSize)
typedTerm tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

exprP :: TokenParser () -> Parser (Expr SymbSize)
exprP tp@TokenParser{..} =
  choice . map try $
    [ funCallE
    , unOpE
    , binOpE
    , constE
    , varE
    ]
 where
  varE :: Parser (Expr SymbSize)
  varE = VarE <$> identifier

  constE :: Parser (Expr SymbSize)
  constE = do
    reserved "const"
    (val, ty) <- typedTerm tp integer
    return ConstE{val, ty}

  funCallKind :: Parser FunctionCallKind
  funCallKind = primitiveCall <|> functionCall
   where
    primitiveCall = do
      prim_name <- char '@' *> identifier
      prim_params <- squares (commaSep identifier)
      return PrimitiveCall{..}
    functionCall = FunctionCall <$> identifier

  funCallE :: Parser (Expr SymbSize)
  funCallE = do
    fun_kind <- funCallKind
    args <- parens $ commaSep identifier
    return FunCallE{fun_kind, args}

  unOp :: Parser UnOp
  unOp =
    operator >>= \case
      "!" -> return NotOp
      _ -> fail "invalid unary operator"

  unOpE :: Parser (Expr SymbSize)
  unOpE = do
    un_op <- unOp
    arg <- identifier
    return UnOpE{un_op, arg}

  binOp :: Parser BinOp
  binOp =
    operator >>= \case
      "+" -> return AddOp
      "<=" -> return LEqOp
      "&&" -> return AndOp
      _ -> fail "invalid binary operator"

  binOpE :: Parser (Expr SymbSize)
  binOpE = do
    lhs <- identifier
    bin_op <- binOp
    rhs <- identifier
    return BinOpE{bin_op, lhs, rhs}

stmtP :: TokenParser () -> Parser (Stmt SymbSize)
stmtP tp@TokenParser{..} = SeqS <$> (someStmt <|> return [])
 where
  someStmt :: Parser [Stmt SymbSize]
  someStmt = (:) <$> exprS <*> many (try (semi *> exprS))

  exprS :: Parser (Stmt SymbSize)
  exprS = do
    rets <- commaSep1 identifier
    reservedOp "<-"
    expr <- exprP tp
    return ExprS{rets, expr}

funDef :: TokenParser () -> Parser (FunDef SymbSize)
funDef tp@TokenParser{..} = do
  reserved "def"
  fun_name <- identifier
  (param_names, param_types) <- unzip <$> parens (commaSep (typedTerm tp identifier))
  body_stmt <- stmtP tp
  _ <- semi
  reserved "return"
  (ret_names, ret_types) <- unzip <$> commaSep (typedTerm tp identifier)
  reserved "end"
  let mbody = Just FunBody{..}
  return FunDef{..}

funDecl :: TokenParser () -> Parser (FunDef SymbSize)
funDecl tp@TokenParser{..} = do
  reserved "declare"
  fun_name <- identifier
  param_types <- parens (commaSep (varType tp))
  reservedOp "->"
  -- single type as is, or multiple as a tuple
  ret_types <- ((: []) <$> varType tp) <|> parens (commaSep (varType tp))
  return FunDef{mbody = Nothing, ..}

program :: TokenParser () -> Parser (Program SymbSize)
program tp@TokenParser{..} = do
  funCtx <- mkFunDefCtx <$> many (funDef tp <|> funDecl tp)
  stmt <- stmtP tp
  return Program{..}

programParser :: Parser (Program SymbSize)
programParser = program protoLangTokenParser

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

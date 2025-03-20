{-# LANGUAGE RecordWildCards #-}

module QCompose.ProtoLang.Parser where

import Data.Either (isRight)
import Data.Functor (($>))
import qualified Data.Number.Symbolic as Sym
import Text.Parsec (ParseError, choice, eof, many, parse, try, (<|>))
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
import QCompose.Utils.Printing

-- TODO unify
subroutines :: [(Primitive, Ident)]
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
  funCallKind = oracleCall <|> subroutineCall <|> functionCall
   where
    oracleCall = reserved "Oracle" $> OracleCall
    subroutineCall =
      choice
        [ reserved sname $> PrimitiveCall sub
        | (sub, sname) <- subroutines
        ]
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
  param_binds <- parens $ commaSep $ typedTerm tp identifier
  reserved "do"
  body <- stmtP tp
  _ <- semi
  reserved "return"
  ret_binds <- commaSep $ typedTerm tp identifier
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

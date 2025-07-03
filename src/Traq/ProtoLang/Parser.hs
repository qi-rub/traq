{-# LANGUAGE RecordWildCards #-}

module Traq.ProtoLang.Parser (
  -- * Parsers
  CanParsePrimitive (..),
  programParser,
  parseCode,
  parseProgram,
  parseFunDef,
  parseStmt,

  -- * Helpers
  isValidIdentifier,
) where

import Data.Either (isRight)
import Data.Functor (($>))
import Data.Void (Void)
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
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.ProtoLang.Syntax

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
        , -- operators
          "not"
        ]
    , reservedOpNames = [":", "<-", "->"]
    }

protoLangTokenParser :: TokenParser st
protoLangTokenParser = makeTokenParser protoLangDef

-- | Support for parsing arbitrary primitives.
class CanParsePrimitive primT where
  primitiveParser :: TokenParser () -> Parser primT

instance CanParsePrimitive Void where
  primitiveParser _ = fail "no parse"

symbSize :: TokenParser () -> Parser SymbSize
symbSize TokenParser{..} = (Sym.con . fromIntegral <$> integer) <|> (Sym.var <$> identifier)

varType :: TokenParser () -> Parser (VarType SymbSize)
varType tp@TokenParser{..} = boolType <|> finType
 where
  boolType = reserved "Bool" $> Fin (Sym.con 2)
  finType = reserved "Fin" >> Fin <$> angles (symbSize tp)

typedTerm :: TokenParser () -> Parser a -> Parser (a, VarType SymbSize)
typedTerm tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

exprP :: forall primT. (CanParsePrimitive primT) => TokenParser () -> Parser (Expr primT SymbSize)
exprP tp@TokenParser{..} =
  choice . map try $
    [ funCallE
    , unOpE
    , binOpE
    , constE
    , varE
    ]
 where
  varE :: Parser (Expr primT SymbSize)
  varE = BasicExprE . VarE <$> identifier

  constE :: Parser (Expr primT SymbSize)
  constE = do
    reserved "const"
    (val, ty) <- typedTerm tp integer
    return $ BasicExprE ConstE{val, ty}

  funCallKind :: Parser (FunctionCallKind primT)
  funCallKind = primitiveCall <|> functionCall
   where
    primitiveCall = PrimitiveCall <$> primitiveParser tp
    functionCall = FunctionCall <$> identifier

  funCallE :: Parser (Expr primT SymbSize)
  funCallE = do
    fun_kind <- funCallKind
    args <- parens $ commaSep identifier
    return FunCallE{fun_kind, args}

  unOp :: Parser UnOp
  unOp =
    (reserved "not" $> NotOp)
      <|> ( operator >>= \case
              "!" -> return NotOp
              _ -> fail "invalid unary operator"
          )

  unOpE :: Parser (Expr primT SymbSize)
  unOpE = do
    un_op <- unOp
    operand <- VarE <$> identifier
    return $ BasicExprE UnOpE{un_op, operand}

  binOp :: Parser BinOp
  binOp =
    operator >>= \case
      "+" -> return AddOp
      "<=" -> return LEqOp
      "&&" -> return AndOp
      _ -> fail "invalid binary operator"

  binOpE :: Parser (Expr primT SymbSize)
  binOpE = do
    lhs <- VarE <$> identifier
    bin_op <- binOp
    rhs <- VarE <$> identifier
    return $ BasicExprE BinOpE{bin_op, lhs, rhs}

stmtP :: forall primT. (CanParsePrimitive primT) => TokenParser () -> Parser (Stmt primT SymbSize)
stmtP tp@TokenParser{..} = SeqS <$> (someStmt <|> return [])
 where
  someStmt :: Parser [Stmt primT SymbSize]
  someStmt = (:) <$> exprS <*> many (try (semi *> exprS))

  exprS :: Parser (Stmt primT SymbSize)
  exprS = do
    rets <- commaSep1 identifier
    reservedOp "<-"
    expr <- exprP tp
    return ExprS{rets, expr}

namedFunDef :: (CanParsePrimitive primT) => TokenParser () -> Parser (NamedFunDef primT SymbSize)
namedFunDef tp@TokenParser{..} = do
  reserved "def"
  fun_name <- identifier
  (param_names, param_types) <- unzip <$> parens (commaSep (typedTerm tp identifier))
  reserved "do"
  body_stmt <- stmtP tp
  _ <- semi
  reserved "return"
  (ret_names, ret_types) <- unzip <$> commaSep (typedTerm tp identifier)
  reserved "end"
  let mbody = Just FunBody{..}
  let fun_def = FunDef{..}
  return NamedFunDef{..}

funDecl :: TokenParser () -> Parser (NamedFunDef primT SymbSize)
funDecl tp@TokenParser{..} = do
  reserved "declare"
  fun_name <- identifier
  param_types <- parens (commaSep (varType tp))
  reservedOp "->"
  -- single type as is, or multiple as a tuple
  ret_types <- ((: []) <$> varType tp) <|> parens (commaSep (varType tp))
  let fun_def = FunDef{mbody = Nothing, ..}
  return NamedFunDef{..}

program :: (CanParsePrimitive primT) => TokenParser () -> Parser (Program primT SymbSize)
program tp@TokenParser{..} = do
  fs <- many (namedFunDef tp <|> funDecl tp)
  let funCtx = Ctx.fromList [(fun_name f, fun_def f) | f <- fs]
  stmt <- stmtP tp
  return Program{..}

programParser :: (CanParsePrimitive primT) => Parser (Program primT SymbSize)
programParser = program protoLangTokenParser

parseCode :: (TokenParser () -> Parser a) -> String -> Either ParseError a
parseCode parser = parse (whiteSpace p *> parser p <* eof) ""
 where
  p = protoLangTokenParser

parseProgram :: (CanParsePrimitive primT) => String -> Either ParseError (Program primT SymbSize)
parseProgram = parseCode program

parseFunDef :: (CanParsePrimitive primT) => String -> Either ParseError (NamedFunDef primT SymbSize)
parseFunDef = parseCode namedFunDef

parseStmt :: (CanParsePrimitive primT) => String -> Either ParseError (Stmt primT SymbSize)
parseStmt = parseCode stmtP

isValidIdentifier :: String -> Bool
isValidIdentifier = isRight . parse (identifier protoLangTokenParser) ""

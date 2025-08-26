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

import Traq.Prelude
import Traq.ProtoLang.Syntax

protoLangDef :: LanguageDef st
protoLangDef =
  emptyDef
    { commentLine = "//"
    , reservedNames =
        [ -- statements
          "const"
        , "update"
        , "set"
        , "loop"
        , -- functions
          "def"
        , "declare"
        , "do"
        , "end"
        , "return"
        , -- types
          "Fin"
        , "Bool"
        , "Arr"
        , -- operators
          "not"
        ]
    , reservedOpNames = [":", "<-", "->", "?"]
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
varType tp@TokenParser{..} = boolType <|> finType <|> arrType
 where
  boolType = reserved "Bool" $> Fin (Sym.con 2)
  finType = reserved "Fin" >> Fin <$> angles (symbSize tp)
  arrType = do
    reserved "Arr"
    angles $ do
      n <- symbSize tp
      comma
      ty <- varType tp
      pure $ Arr n ty

typedTerm :: TokenParser () -> Parser a -> Parser (a, VarType SymbSize)
typedTerm tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

exprP :: forall primT. (CanParsePrimitive primT) => TokenParser () -> Parser (Expr primT SymbSize)
exprP tp@TokenParser{..} =
  choice . map try $
    [ parens (exprP tp)
    , primCallE
    , funCallE
    , ternaryE
    , unOpE
    , binOpE
    , binOpFlippedE
    , constE
    , dynIndexE
    , indexE
    , updateArrE
    , loopE
    , varE
    ]
 where
  varE :: Parser (Expr primT SymbSize)
  varE = BasicExprE . VarE <$> identifier

  constE :: Parser (Expr primT SymbSize)
  constE = do
    reserved "const"
    (val, ty) <- typedTerm tp integer
    return $ BasicExprE ConstE{val = FinV (fromInteger val), ty}

  primCallE :: Parser (Expr primT SymbSize)
  primCallE = PrimCallE <$> primitiveParser tp

  funCallE :: Parser (Expr primT SymbSize)
  funCallE = do
    fname <- identifier
    args <- parens $ commaSep identifier
    return FunCallE{fname, args}

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
      "*" -> return MulOp
      "-" -> return SubOp
      "^" -> return XorOp
      "<=" -> return LEqOp
      "&&" -> return AndOp
      "<" -> return LtOp
      _ -> fail "invalid binary operator"

  binOpE :: Parser (Expr primT SymbSize)
  binOpE = do
    lhs <- VarE <$> identifier
    bin_op <- binOp
    rhs <- VarE <$> identifier
    return $ BasicExprE BinOpE{bin_op, lhs, rhs}

  binOpFlipped :: Parser BinOp
  binOpFlipped =
    operator >>= \case
      ">" -> return LtOp
      ">=" -> return LEqOp
      _ -> fail "invalid flipped binary operator"

  binOpFlippedE :: Parser (Expr primT SymbSize)
  binOpFlippedE = do
    rhs <- VarE <$> identifier
    bin_op <- binOpFlipped
    lhs <- VarE <$> identifier
    return $ BasicExprE BinOpE{bin_op, lhs, rhs}

  indexE :: Parser (Expr primT SymbSize)
  indexE = do
    arr_expr <- VarE <$> identifier
    ix_val <- brackets $ symbSize tp
    return $ BasicExprE IndexE{arr_expr, ix_val}

  dynIndexE :: Parser (Expr primT SymbSize)
  dynIndexE = do
    arr_expr <- VarE <$> identifier
    ix_expr <- VarE <$> brackets identifier
    return $ BasicExprE DynIndexE{arr_expr, ix_expr}

  updateArrE :: Parser (Expr primT SymbSize)
  updateArrE = do
    reserved "update" <|> reserved "set"
    arr_expr <- VarE <$> identifier
    ix_expr <- VarE <$> brackets identifier
    "=" <- operator
    rhs <- VarE <$> identifier
    return $ BasicExprE UpdateArrE{arr_expr, ix_expr, rhs}

  ternaryE :: Parser (Expr primT SymbSize)
  ternaryE = do
    branch <- VarE <$> identifier
    reservedOp "?"
    lhs <- VarE <$> identifier
    reservedOp ":"
    rhs <- VarE <$> identifier
    return $ BasicExprE $ TernaryE{branch, lhs, rhs}

  loopE :: Parser (Expr primT SymbSize)
  loopE = do
    reserved "loop"
    initial_args <- parens $ commaSep (VarE <$> identifier)
    loop_body_fun <- identifier
    return $ LoopE{initial_args, loop_body_fun}

randExprP :: forall primT. (CanParsePrimitive primT) => TokenParser () -> Parser (Expr primT SymbSize)
randExprP tp@TokenParser{..} = choice $ map try [uniformE, biasedCoinE]
 where
  uniformE :: Parser (Expr primT SymbSize)
  uniformE = do
    reserved "uniform"
    sample_ty <- varType tp
    return UniformRandomE{..}

  biasedCoinE :: Parser (Expr primT SymbSize)
  biasedCoinE = do
    reserved "coin"
    prob_one <- float
    return BiasedCoinE{..}

stmtP :: forall primT. (CanParsePrimitive primT) => TokenParser () -> Parser (Stmt primT SymbSize)
stmtP tp@TokenParser{..} = SeqS <$> many exprS
 where
  exprS :: Parser (Stmt primT SymbSize)
  exprS = do
    rets <- commaSep1 identifier
    expr <- (reserved "<-$" *> randExprP tp) <|> (reserved "<-" *> exprP tp)
    semi
    return ExprS{rets, expr}

namedFunDef :: (CanParsePrimitive primT) => TokenParser () -> Parser (NamedFunDef primT SymbSize)
namedFunDef tp@TokenParser{..} = do
  reserved "def"
  fun_name <- identifier
  (param_names, param_types) <- unzip <$> parens (commaSep (typedTerm tp identifier))
  reserved "->"
  ret_types <- ((: []) <$> varType tp) <|> parens (commaSep (varType tp))
  reserved "do"
  body_stmt <- stmtP tp
  reserved "return"
  ret_names <- commaSep identifier
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
  reserved "end"
  let fun_def = FunDef{mbody = Nothing, ..}
  return NamedFunDef{..}

program :: (CanParsePrimitive primT) => TokenParser () -> Parser (Program primT SymbSize)
program tp = do
  fs <- many (namedFunDef tp <|> funDecl tp)
  return $ Program fs

programParser :: (CanParsePrimitive primT) => Parser (Program primT SymbSize)
programParser = whiteSpace p *> program protoLangTokenParser <* eof
 where
  p = protoLangTokenParser

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

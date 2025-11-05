{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.ProtoLang.Parser (
  -- * Parsers
  Parseable (..),
  programParser,
  parseCode,
  parseProgram,
  parseFunDef,
  parseStmt,
  varType,

  -- * Helpers
  isValidIdentifier,
) where

import Data.Either (isRight)
import Data.Functor (($>))
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

import Lens.Micro.GHC

import qualified Traq.Data.Symbolic as Sym

import Traq.Prelude
import Traq.ProtoLang.Syntax

-- | Basic symbolic type
type SymbSize = Sym.Sym SizeT

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
        , "BitVec"
        , "Word"
        , "Tup"
        , -- operators
          "not"
        ]
    , reservedOpNames = [":", "<-", "->", "?"]
    }

protoLangTokenParser :: TokenParser st
protoLangTokenParser = makeTokenParser protoLangDef

class Parseable ext where
  parseE :: TokenParser () -> Parser ext

instance Parseable (Core SymbSize precT) where
  parseE _ = fail "no parse"

symbSize :: TokenParser () -> Parser SymbSize
symbSize TokenParser{..} = (Sym.con . fromIntegral <$> integer) <|> (Sym.var <$> identifier)

varType :: TokenParser () -> Parser (VarType SymbSize)
varType tp@TokenParser{..} = boolType <|> finType <|> bitvecType <|> arrType <|> tupleType
 where
  boolType = reserved "Bool" $> Fin (Sym.con 2)
  finType = reserved "Fin" >> Fin <$> angles (symbSize tp)
  bitvecType = (reserved "BitVec" <|> reserved "Bitvec" <|> reserved "Word") >> Bitvec <$> angles (symbSize tp)
  arrType = do
    reserved "Arr"
    angles $ do
      n <- symbSize tp
      comma
      ty <- varType tp
      pure $ Arr n ty
  tupleType = do
    reserved "Tup"
    angles $ do
      tys <- commaSep1 $ varType tp
      pure $ Tup tys

typedTerm :: TokenParser () -> Parser a -> Parser (a, VarType SymbSize)
typedTerm tp@TokenParser{..} pa = (,) <$> pa <*> (reservedOp ":" *> varType tp)

instance (Parseable ext, SizeType ext ~ SymbSize) => Parseable (Expr ext) where
  parseE tp@TokenParser{..} =
    choice . map try $
      [ parens (exprP tp)
      , primCallE
      , funCallE
      , ternaryE
      , unOpE
      , binOpE
      , binOpFlippedE
      , defaultE
      , constE
      , dynIndexE
      , indexE
      , updateArrE
      , loopE
      , varE
      ]
   where
    varE :: Parser (Expr ext)
    varE = BasicExprE . VarE <$> identifier

    defaultE :: Parser (Expr ext)
    defaultE = do
      reserved "default"
      colon
      ty <- varType tp
      return $ BasicExprE DefaultE{ty}

    constE :: Parser (Expr ext)
    constE = do
      reserved "const"
      (val, ty) <- typedTerm tp integer
      return $ BasicExprE ConstE{val = FinV (fromInteger val), ty}

    primCallE :: Parser (Expr ext)
    primCallE = PrimCallE <$> parseE tp

    funCallE :: Parser (Expr ext)
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

    unOpE :: Parser (Expr ext)
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
        "==" -> return EqOp
        -- "!=" -> return $ (Not (EqOp))
        _ -> fail "invalid binary operator"

    binOpE :: Parser (Expr ext)
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

    binOpFlippedE :: Parser (Expr ext)
    binOpFlippedE = do
      rhs <- VarE <$> identifier
      bin_op <- binOpFlipped
      lhs <- VarE <$> identifier
      return $ BasicExprE BinOpE{bin_op, lhs, rhs}

    indexE :: Parser (Expr ext)
    indexE = do
      arr_expr <- VarE <$> identifier
      ix_val <- brackets $ symbSize tp
      return $ BasicExprE IndexE{arr_expr, ix_val}

    dynIndexE :: Parser (Expr ext)
    dynIndexE = do
      arr_expr <- VarE <$> identifier
      ix_expr <- VarE <$> brackets identifier
      return $ BasicExprE DynIndexE{arr_expr, ix_expr}

    updateArrE :: Parser (Expr ext)
    updateArrE = do
      reserved "update" <|> reserved "set"
      arr_expr <- VarE <$> identifier
      ix_expr <- VarE <$> brackets identifier
      "=" <- operator
      rhs <- VarE <$> identifier
      return $ BasicExprE UpdateArrE{arr_expr, ix_expr, rhs}

    ternaryE :: Parser (Expr ext)
    ternaryE = do
      branch <- VarE <$> identifier
      reservedOp "?"
      lhs <- VarE <$> identifier
      reservedOp ":"
      rhs <- VarE <$> identifier
      return $ BasicExprE $ TernaryE{branch, lhs, rhs}

    loopE :: Parser (Expr ext)
    loopE = do
      reserved "loop"
      initial_args <- parens $ commaSep identifier
      loop_body_fun <- identifier
      return $ LoopE{initial_args, loop_body_fun}

exprP :: forall ext. (Parseable ext, SizeType ext ~ SymbSize) => TokenParser () -> Parser (Expr ext)
exprP = parseE

instance Parseable (DistrExpr SymbSize) where
  parseE tp@TokenParser{..} = choice $ map try [uniformE, bernoulliE]
   where
    uniformE :: Parser (DistrExpr SymbSize)
    uniformE = do
      reserved "uniform"
      colon
      sample_ty <- varType tp
      return UniformE{..}

    bernoulliE :: Parser (DistrExpr SymbSize)
    bernoulliE = do
      reserved "bernoulli"
      prob_one <- brackets float
      return BernoulliE{..}

distrExprP :: TokenParser () -> Parser (DistrExpr SymbSize)
distrExprP = parseE

instance (Parseable ext, SizeType ext ~ SymbSize) => Parseable (Stmt ext) where
  parseE tp@TokenParser{..} = SeqS <$> many exprS
   where
    exprS :: Parser (Stmt ext)
    exprS = do
      rets <- commaSep1 identifier
      expr <- (reserved "<-$" *> distrExprP tp <&> RandomSampleE) <|> (reserved "<-" *> exprP tp)
      semi
      return ExprS{rets, expr}

stmtP :: forall ext. (Parseable ext, SizeType ext ~ SymbSize) => TokenParser () -> Parser (Stmt ext)
stmtP = parseE

namedFunDef :: (Parseable ext, SizeType ext ~ SymbSize) => TokenParser () -> Parser (NamedFunDef ext)
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

funDecl :: (SizeType ext ~ SymbSize) => TokenParser () -> Parser (NamedFunDef ext)
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

program :: (Parseable ext, SizeType ext ~ SymbSize) => TokenParser () -> Parser (Program ext)
program tp = do
  fs <- many (namedFunDef tp <|> funDecl tp)
  return $ Program fs

programParser :: (Parseable ext, SizeType ext ~ SymbSize) => Parser (Program ext)
programParser = whiteSpace p *> program protoLangTokenParser <* eof
 where
  p = protoLangTokenParser

parseCode :: (TokenParser () -> Parser a) -> String -> Either ParseError a
parseCode parser = parse (whiteSpace p *> parser p <* eof) ""
 where
  p = protoLangTokenParser

parseProgram :: (Parseable ext, SizeType ext ~ SymbSize) => String -> Either ParseError (Program ext)
parseProgram = parseCode program

parseFunDef :: (Parseable ext, SizeType ext ~ SymbSize) => String -> Either ParseError (NamedFunDef ext)
parseFunDef = parseCode namedFunDef

parseStmt :: (Parseable ext, SizeType ext ~ SymbSize) => String -> Either ParseError (Stmt ext)
parseStmt = parseCode stmtP

isValidIdentifier :: String -> Bool
isValidIdentifier = isRight . parse (identifier protoLangTokenParser) ""

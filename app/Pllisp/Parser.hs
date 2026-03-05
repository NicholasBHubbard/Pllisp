{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Parser where

import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty


import Data.Void (Void)
import qualified Data.Text as T

import           Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Pos as MP.P
import qualified Text.Megaparsec.Char as MP.C
import qualified Text.Megaparsec.Char.Lexer as MP.C.L

-- CORE

type Parser = MP.Parsec Void T.Text

parseProgram :: T.Text -> Either (MP.ParseErrorBundle T.Text Void) CST.CST
parseProgram = MP.parse programParser ""

programParser :: Parser CST.CST
programParser = MP.label "program" $ sc *> MP.many exprParser <* MP.eof

exprParser :: Parser CST.Expr
exprParser = MP.label "expression" $ located $ MP.choice
  [ exprLamParser
  , exprLetParser
  , exprIfParser
  , exprTypeParser
  , exprAppParser
  , exprBoolParser
  , exprLitParser
  , exprSymParser
  ]

exprLamParser :: Parser CST.ExprF
exprLamParser = MP.label "lambda" $ MP.try $ parens $ do
  _ <- ident' "LAM"
  args <- parens (MP.many tsymbolParser)
  rettype <- MP.optional typeParser
  body <- exprParser
  pure $ CST.ExprLam args rettype body

exprAppParser :: Parser CST.ExprF
exprAppParser = MP.label "application" $ parens $ do
  fun <- exprParser
  args <- MP.many exprParser
  pure $ CST.ExprApp fun args

exprLetParser :: Parser CST.ExprF
exprLetParser = MP.label "let" $ MP.try $ parens $ do
  _ <- ident' "LET"
  let letBindingParser = parens $ (,) <$> tsymbolParser <*> exprParser
  bindings <- parens (MP.many letBindingParser)
  body <- exprParser
  pure $ CST.ExprLet bindings body

exprIfParser :: Parser CST.ExprF
exprIfParser = MP.label "if" $ MP.try $ parens $ do
  _ <- ident' "IF"
  cond <- exprParser
  then' <- exprParser
  else' <- exprParser
  pure $ CST.ExprIf cond then' else'

exprTypeParser :: Parser CST.ExprF
exprTypeParser = MP.label "type" $ MP.try $ parens $ do
  _ <- ident' "TYPE"
  name <- symbolParser
  params <- parens (MP.many symbolParser)
  ctors <- MP.many dataConParser
  pure $ CST.ExprType name params ctors

dataConParser :: Parser CST.DataCon
dataConParser = MP.label "data constructor" $ parens $ do
  name <- symbolParser
  args <- MP.many typeParserInCtor
  pure $ CST.DataCon name args

-- Type parser for inside constructor definitions (handles type variables)
typeParserInCtor :: Parser Ty.Type
typeParserInCtor = MP.choice
  [ MP.C.char '%' *> typeParserBody  -- %INT, %Maybe, %(List a)
  , Ty.TyCon <$> symbolParser <*> pure []  -- bare type variable: a, b, etc.
  ]

typeParserBody :: Parser Ty.Type
typeParserBody = MP.choice
  [ parens $ do  -- %(List a) or %(Either a b)
      name <- ident
      args <- MP.many typeParserInCtor
      pure $ Ty.TyCon name args
  , ident >>= \t -> case t of
      "INT"  -> pure Ty.TyInt
      "FLT"  -> pure Ty.TyFlt
      "STR"  -> pure Ty.TyStr
      "BOOL" -> pure Ty.TyBool
      other  -> pure (Ty.TyCon other [])
  ]

exprSymParser :: Parser CST.ExprF
exprSymParser = MP.label "symbol" $ CST.ExprSym <$> symbolParser

exprBoolParser :: Parser CST.ExprF
exprBoolParser = MP.label "boolean" $ CST.ExprBool <$> boolParser

exprLitParser :: Parser CST.ExprF
exprLitParser = MP.label "literal" $ CST.ExprLit <$> litParser

tsymbolParser :: Parser CST.TSymbol
tsymbolParser = MP.label "typed symbol" $ MP.choice
  [ parens $ CST.TSymbol <$> symbolParser <*> (Just <$> typeParser)
  , CST.TSymbol <$> symbolParser <*> pure Nothing
  ]

symbolParser :: Parser CST.Symbol
symbolParser = MP.label "symbol" $ do
  s <- ident
  if s `elem` keywords
    then fail "reserved word"
    else pure s

typeParser :: Parser Ty.Type
typeParser = MP.C.char '%' *> (ident >>= \t -> case t of
  "INT"  -> pure Ty.TyInt
  "FLT"  -> pure Ty.TyFlt
  "STR"  -> pure Ty.TyStr
  "BOOL" -> pure Ty.TyBool
  other  -> pure (Ty.TyCon other [])
  )

boolParser :: Parser Bool
boolParser = MP.label "boolean value" $ do
  s <- MP.lookAhead ident
  case s of
    "TRUE"  -> ident *> pure True
    "FALSE" -> ident *> pure False
    _       -> fail "boolean"

litParser :: Parser CST.Literal
litParser = MP.label "literal value" $ lexeme $ MP.choice
  [ CST.LitFlt <$> MP.try MP.C.L.float
  , CST.LitInt <$> MP.C.L.decimal
  , CST.LitStr . T.pack <$> (MP.C.char '"' *> MP.manyTill MP.C.L.charLiteral (MP.C.char '"'))
  ]

-- HELPERS

keywords :: [T.Text]
keywords = ["LAM", "LET", "IF", "TRUE", "FALSE", "TYPE"]

ident :: Parser T.Text
ident = MP.label "identifier" $ lexeme $ do
  first <- MP.C.char '_' <|> MP.C.letterChar
  rest  <- MP.many (MP.C.char '_' <|> MP.C.char '-' <|> MP.C.alphaNumChar)
  pure $ T.toUpper (T.pack (first:rest))

ident' :: T.Text -> Parser T.Text
ident' t = do
  let t' = T.toUpper t
  s <- ident
  if s == t' then pure s else fail ("expected " <> T.unpack t')

lexeme :: Parser a -> Parser a
lexeme = MP.C.L.lexeme sc

parens :: Parser a -> Parser a
parens = MP.between (MP.C.L.symbol sc "(") (MP.C.L.symbol sc ")")

sc :: Parser ()
sc = MP.C.L.space MP.C.space1 (MP.C.L.skipLineComment "#") MP.empty

located :: Parser a -> Parser (Loc.Located a)
located p =
  let toPos sp = Loc.Pos (MP.sourceName sp) (MP.P.unPos (MP.sourceLine sp)) (MP.P.unPos (MP.sourceColumn sp))
  in do
    start <- MP.getSourcePos
    x <- p
    end <- MP.getSourcePos
    pure $ Loc.Located (Loc.Span (toPos start) (toPos end)) x

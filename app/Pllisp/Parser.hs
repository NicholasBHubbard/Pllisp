{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Parser where

import qualified Pllisp.CST as CST
import qualified Pllisp.SExpr as SExpr
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

parseProgram :: FilePath -> T.Text -> Either (MP.ParseErrorBundle T.Text Void) CST.Program
parseProgram fp = MP.parse programParser fp

programParser :: Parser CST.Program
programParser = MP.label "program" $ do
  sc
  mName <- MP.optional moduleParser
  imports <- MP.many importParser
  exprs <- MP.many exprParser
  MP.eof
  pure (CST.Program mName imports exprs)

moduleParser :: Parser CST.Symbol
moduleParser = MP.label "module declaration" $ MP.try $ parens $ do
  _ <- ident' "MODULE"
  symbolParser

importParser :: Parser CST.Import
importParser = MP.label "import declaration" $ MP.try $ parens $ do
  _ <- ident' "IMPORT"
  modName <- symbolParser
  mNext <- MP.optional (Left <$> MP.try symbolParser MP.<|> Right <$> parens (MP.many symbolParser))
  case mNext of
    Nothing           -> pure (CST.Import modName modName [])
    Just (Right uqs)  -> pure (CST.Import modName modName uqs)
    Just (Left alias) -> do
      unquals <- MP.option [] $ parens (MP.many symbolParser)
      pure (CST.Import modName alias unquals)

exprParser :: Parser CST.Expr
exprParser = MP.label "expression" $ located $ MP.choice
  [ exprLamParser
  , exprLetParser
  , exprIfParser
  , exprTypeParser
  , exprClsParser
  , exprInstParser
  , exprCaseParser
  , exprAppParser
  , exprUnitParser
  , exprBoolParser
  , exprLitParser
  , exprUSymParser
  , exprSymParser
  ]

exprLamParser :: Parser CST.ExprF
exprLamParser = MP.label "lambda" $ MP.try $ parens $ do
  _ <- ident' "LAM"
  lamList <- parens lamListParser
  rettype <- MP.optional typeParser
  body <- exprParser
  pure $ CST.ExprLam lamList rettype body

lamListParser :: Parser CST.LamList
lamListParser = do
  required <- MP.many (MP.try regularParam)
  extra <- MP.choice
    [ MP.try restParamParser
    , MP.try optParamParser
    , MP.try keyParamParser
    , pure CST.NoExtra
    ]
  pure $ CST.LamList required extra
  where
    -- A regular param that is not &REST or &KEY
    regularParam = do
      ts <- tsymbolParser
      case CST.symName ts of
        "&REST" -> fail "not a regular param"
        "&KEY"  -> fail "not a regular param"
        _       -> pure ts

    restParamParser = do
      _ <- ident' "&REST"
      param <- tsymbolParser
      pure $ CST.RestParam param

    optParamParser = do
      _ <- MP.C.char '%' *> (ident >>= \t -> if t == "OPT" then pure () else fail "expected OPT")
      opts <- MP.many defaultParamParser
      pure $ CST.OptParams opts

    keyParamParser = do
      _ <- ident' "&KEY"
      keys <- MP.many defaultParamParser
      pure $ CST.KeyParams keys

    defaultParamParser = parens $ (,) <$> tsymbolParser <*> exprParser

exprAppParser :: Parser CST.ExprF
exprAppParser = MP.label "application" $ parens $ do
  fun <- exprParser
  args <- MP.many appArgParser
  pure $ CST.ExprApp fun args

-- | Parse an application argument, which may be a &key pair or a regular expr.
appArgParser :: Parser CST.Expr
appArgParser = MP.choice
  [ MP.try keyArgParser
  , exprParser
  ]

keyArgParser :: Parser CST.Expr
keyArgParser = located $ do
  _ <- ident' "&KEY"
  name <- symbolParser
  val <- exprParser
  pure $ CST.ExprKeyArg name val

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

exprCaseParser :: Parser CST.ExprF
exprCaseParser = MP.label "case" $ MP.try $ parens $ do
  _ <- ident' "CASE"
  scrutinee <- exprParser
  arms <- MP.many $ parens $ (,) <$> patternParser <*> exprParser
  pure $ CST.ExprCase scrutinee arms

exprClsParser :: Parser CST.ExprF
exprClsParser = MP.label "cls" $ MP.try $ parens $ do
  _ <- ident' "CLS"
  name <- symbolParser
  supers <- parens (MP.many symbolParser)
  tvars <- parens (MP.many symbolParser)
  methods <- MP.many classMethodParser
  pure $ CST.ExprCls name tvars supers methods
  where
    classMethodParser = parens $ do
      mname <- symbolParser
      tys <- MP.many typeParser
      pure $ CST.ClassMethod mname (init tys) (last tys)

exprInstParser :: Parser CST.ExprF
exprInstParser = MP.label "inst" $ MP.try $ parens $ do
  _ <- ident' "INST"
  className <- symbolParser
  ty <- typeParser
  methods <- MP.many instMethodParser
  pure $ CST.ExprInst className ty methods
  where
    instMethodParser = parens $ (,) <$> symbolParser <*> exprParser

patternParser :: Parser CST.Pattern
patternParser = MP.label "pattern" $ MP.choice
  [ MP.try $ parens $ CST.PatCon <$> symbolParser <*> MP.many patternParser
  , CST.PatBool <$> boolParser
  , CST.PatLit  <$> litParser
  , CST.PatLit . CST.LitUSym <$> (lexeme $ MP.C.char ':' *> rawIdent)
  , fmap (\s -> if s == "_" then CST.PatWild else CST.PatVar s) symbolParser
  ]

dataConParser :: Parser CST.DataCon
dataConParser = MP.label "data constructor" $ parens $ do
  name <- symbolParser
  args <- MP.many typeParserInCtor
  pure $ CST.DataCon name args Nothing

-- Type parser for inside constructor definitions (handles type variables)
typeParserInCtor :: Parser Ty.Type
typeParserInCtor = MP.choice
  [ MP.C.char '%' *> typeParserBody  -- %INT, %Maybe, %(List a)
  , Ty.TyCon <$> symbolParser <*> pure []  -- bare type variable: a, b, etc.
  ]

typeParserBody :: Parser Ty.Type
typeParserBody = MP.choice
  [ parens $ do
      mArrow <- MP.optional (MP.try (lexeme (MP.C.string "->") *> pure ()))
      case mArrow of
        Just _ -> do
          tys <- MP.some typeElem
          pure $ Ty.TyFun (init tys) (last tys)
        Nothing -> do
          name <- ident
          args <- MP.many typeParserInCtor
          pure $ Ty.TyCon name args
  , ident >>= \t -> case t of
      "INT"  -> pure Ty.TyInt
      "FLT"  -> pure Ty.TyFlt
      "STR"  -> pure Ty.TyStr
      "BOOL" -> pure Ty.TyBool
      "UNIT"  -> pure Ty.TyUnit
      "RX"    -> pure Ty.TyRx
      "USYM"  -> pure Ty.TyUSym
      "SYNTAX" -> pure Ty.TySyntax
      other   -> pure (Ty.TyCon other [])
  ]

-- | A type element inside an arrow type: handles bare names, parens, and % prefix.
typeElem :: Parser Ty.Type
typeElem = MP.choice
  [ MP.C.char '%' *> typeParserBody
  , parens $ do
      mArrow <- MP.optional (MP.try (lexeme (MP.C.string "->") *> pure ()))
      case mArrow of
        Just _ -> do
          tys <- MP.some typeElem
          pure $ Ty.TyFun (init tys) (last tys)
        Nothing -> do
          name <- ident
          args <- MP.many typeParserInCtor
          pure $ Ty.TyCon name args
  , ident >>= \t -> case t of
      "INT"  -> pure Ty.TyInt
      "FLT"  -> pure Ty.TyFlt
      "STR"  -> pure Ty.TyStr
      "BOOL" -> pure Ty.TyBool
      "UNIT"  -> pure Ty.TyUnit
      "RX"    -> pure Ty.TyRx
      "USYM"  -> pure Ty.TyUSym
      "SYNTAX" -> pure Ty.TySyntax
      other   -> pure (Ty.TyCon other [])
  ]

exprSymParser :: Parser CST.ExprF
exprSymParser = MP.label "symbol" $ CST.ExprSym <$> symbolParser

exprBoolParser :: Parser CST.ExprF
exprBoolParser = MP.label "boolean" $ CST.ExprBool <$> boolParser

exprUnitParser :: Parser CST.ExprF
exprUnitParser = MP.label "unit" $ do
  s <- MP.lookAhead ident
  case s of
    "UNIT" -> ident *> pure CST.ExprUnit
    _      -> fail "unit"

exprLitParser :: Parser CST.ExprF
exprLitParser = MP.label "literal" $ CST.ExprLit <$> litParser

exprUSymParser :: Parser CST.ExprF
exprUSymParser = MP.label "uninterned symbol" $
  CST.ExprLit . CST.LitUSym <$> (lexeme $ MP.C.char ':' *> rawIdent)

tsymbolParser :: Parser CST.TSymbol
tsymbolParser = MP.label "typed symbol" $ MP.choice
  [ parens $ CST.TSymbol <$> symbolParser <*> (Just <$> typeParser)
  , CST.TSymbol <$> symbolParser <*> pure Nothing
  ]

symbolParser :: Parser CST.Symbol
symbolParser = MP.label "symbol" $ lexeme $ do
  s <- rawIdent
  mDot <- MP.optional (MP.C.char '.' *> rawIdent)
  let sym = case mDot of
        Nothing -> s
        Just q  -> s <> "." <> q
  if sym `elem` keywords
    then fail "reserved word"
    else pure sym

typeParser :: Parser Ty.Type
typeParser = MP.C.char '%' *> typeParserBody

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
  , rxLitParser
  ]

rxLitParser :: Parser CST.Literal
rxLitParser = do
  _ <- MP.C.char '/'
  chunks <- MP.manyTill rxChunk (MP.C.char '/')
  flags <- T.pack <$> MP.many MP.C.lowerChar
  pure (CST.LitRx (T.pack (concat chunks)) flags)
  where
    rxChunk =
          (MP.C.string "\\/" *> pure "/")                                -- \/ → literal /
      <|> ((\c -> ['\\', c]) <$> (MP.C.char '\\' *> MP.anySingle))      -- \X → \X
      <|> ((:[]) <$> MP.anySingle)                                       -- X → X

-- HELPERS

keywords :: [T.Text]
keywords = ["LAM", "LET", "IF", "TRUE", "FALSE", "UNIT", "TYPE", "CASE", "MODULE", "IMPORT"]

rawIdent :: Parser T.Text
rawIdent = do
  first <- MP.C.char '_' <|> MP.C.char '&' <|> MP.C.letterChar
  rest  <- MP.many (MP.C.char '_'
                    <|> MP.try (MP.C.char '-' <* MP.notFollowedBy (MP.C.char '>'))
                    <|> MP.C.char '!' <|> MP.C.char '?' <|> MP.C.alphaNumChar)
  pure $ T.toUpper (T.pack (first:rest))

ident :: Parser T.Text
ident = MP.label "identifier" $ lexeme rawIdent

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

-- SEXPR PARSER

parseSExprs :: FilePath -> T.Text -> Either (MP.ParseErrorBundle T.Text Void) [SExpr.SExpr]
parseSExprs fp = MP.parse sexprsParser fp

sexprsParser :: Parser [SExpr.SExpr]
sexprsParser = sc *> MP.many sexprParser <* MP.eof

sexprParser :: Parser SExpr.SExpr
sexprParser = located $ MP.choice
  [ sexprQuasiParser
  , MP.try sexprSpliceParser
  , sexprUnquoteParser
  , sexprTypeParser'
  , SExpr.SList <$> parens (MP.many sexprParser)
  , litToSExpr <$> lexeme (MP.choice
      [ CST.LitFlt <$> MP.try MP.C.L.float
      , CST.LitInt <$> MP.C.L.decimal
      , CST.LitStr . T.pack <$> (MP.C.char '"' *> MP.manyTill MP.C.L.charLiteral (MP.C.char '"'))
      , rxLitParser
      ])
  , SExpr.SUSym <$> sexprUSymParser
  , SExpr.SAtom <$> sexprAtomParser
  ]

sexprAtomParser :: Parser T.Text
sexprAtomParser = MP.label "atom" $ lexeme $ arrowOp <|> dotAccessor <|> regularAtom
  where
    arrowOp = MP.try (MP.C.string "->" *> pure "->")
    dotAccessor = MP.try $ do
      _ <- MP.C.char '.'
      s <- rawIdent
      pure ("." <> s)
    regularAtom = do
      s <- rawIdent
      mDot <- MP.optional (MP.C.char '.' *> rawIdent)
      pure $ case mDot of
        Nothing -> s
        Just q  -> s <> "." <> q

sexprQuasiParser :: Parser SExpr.SExprF
sexprQuasiParser = do
  _ <- MP.C.char '`'
  SExpr.SQuasi <$> sexprParser

sexprUnquoteParser :: Parser SExpr.SExprF
sexprUnquoteParser = do
  _ <- MP.C.char ','
  SExpr.SUnquote <$> sexprParser

sexprSpliceParser :: Parser SExpr.SExprF
sexprSpliceParser = do
  _ <- MP.C.string ",@"
  SExpr.SSplice <$> sexprParser

sexprTypeParser' :: Parser SExpr.SExprF
sexprTypeParser' = do
  _ <- MP.C.char '%'
  SExpr.SType <$> sexprParser

sexprUSymParser :: Parser T.Text
sexprUSymParser = MP.label "uninterned symbol" $ lexeme $ MP.C.char ':' *> rawIdent

litToSExpr :: CST.Literal -> SExpr.SExprF
litToSExpr (CST.LitInt n)    = SExpr.SInt n
litToSExpr (CST.LitFlt d)    = SExpr.SFlt d
litToSExpr (CST.LitStr s)    = SExpr.SStr s
litToSExpr (CST.LitRx p f)   = SExpr.SRx p f
litToSExpr (CST.LitUSym t)   = SExpr.SUSym t

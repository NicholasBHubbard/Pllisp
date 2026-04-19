{-# LANGUAGE OverloadedStrings #-}

module SExprSpec (spec) where

import Test.Hspec

import qualified Data.Text as T

import qualified Pllisp.CST    as CST
import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.Parser as Parser
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type   as Ty

-- Strip locations from an SExpr tree for comparison
strip :: SExpr.SExpr -> SExpr.SExprF
strip (Loc.Located _ f) = case f of
  SExpr.SList xs     -> SExpr.SList (map stripL xs)
  SExpr.SType inner  -> SExpr.SType (stripL inner)
  SExpr.SQuasi inner -> SExpr.SQuasi (stripL inner)
  SExpr.SUnquote inner -> SExpr.SUnquote (stripL inner)
  SExpr.SSplice inner  -> SExpr.SSplice (stripL inner)
  other -> other

-- Strip but keep the Located wrapper (with dummy span)
stripL :: SExpr.SExpr -> SExpr.SExpr
stripL sx = Loc.Located dummySpan (strip sx)

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)

-- Parse and strip all locations for easy comparison
parseS :: T.Text -> Either String [SExpr.SExprF]
parseS src = case Parser.parseSExprs "<test>" src of
  Left err -> Left (show err)
  Right sexprs -> Right (map strip sexprs)

-- Parse a single sexpr
parseOne :: T.Text -> Either String SExpr.SExprF
parseOne src = case parseS src of
  Left err -> Left err
  Right []    -> Left "no expressions"
  Right (x:_) -> Right x

spec :: Spec
spec = do
  describe "atoms" $ do
    it "symbol" $ do
      parseOne "foo" `shouldBe` Right (SExpr.SAtom "FOO")

    it "symbol with dash" $ do
      parseOne "foo-bar" `shouldBe` Right (SExpr.SAtom "FOO-BAR")

    it "underscore start" $ do
      parseOne "_x" `shouldBe` Right (SExpr.SAtom "_X")

    it "dotted symbol" $ do
      parseOne "Foo.bar" `shouldBe` Right (SExpr.SAtom "FOO.BAR")

    it "keywords are valid atoms" $ do
      parseOne "lam" `shouldBe` Right (SExpr.SAtom "LAM")
      parseOne "let" `shouldBe` Right (SExpr.SAtom "LET")
      parseOne "if" `shouldBe` Right (SExpr.SAtom "IF")

    it "true/false/unit are atoms" $ do
      parseOne "true" `shouldBe` Right (SExpr.SAtom "TRUE")
      parseOne "false" `shouldBe` Right (SExpr.SAtom "FALSE")
      parseOne "unit" `shouldBe` Right (SExpr.SAtom "UNIT")

    it "&rest" $ do
      parseOne "&rest" `shouldBe` Right (SExpr.SAtom "&REST")

  describe "literals" $ do
    it "integer" $ do
      parseOne "42" `shouldBe` Right (SExpr.SInt 42)

    it "float" $ do
      parseOne "3.14" `shouldBe` Right (SExpr.SFlt 3.14)

    it "string" $ do
      parseOne "\"hello\"" `shouldBe` Right (SExpr.SStr "hello")

    it "regex" $ do
      parseOne "/foo/" `shouldBe` Right (SExpr.SRx "foo" "")

    it "regex with flags" $ do
      parseOne "/foo/gi" `shouldBe` Right (SExpr.SRx "foo" "gi")

  describe "lists" $ do
    it "empty list" $ do
      r <- either fail pure $ parseOne "()"
      case r of
        SExpr.SList [] -> pure ()
        _ -> expectationFailure (show r)

    it "list of atoms" $ do
      r <- either fail pure $ parseOne "(a b c)"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SAtom "A"),
                     Loc.Located _ (SExpr.SAtom "B"),
                     Loc.Located _ (SExpr.SAtom "C")] -> pure ()
        _ -> expectationFailure (show r)

    it "nested list" $ do
      r <- either fail pure $ parseOne "(a (b c))"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SAtom "A"),
                     Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom "B"),
                                                 Loc.Located _ (SExpr.SAtom "C")])] -> pure ()
        _ -> expectationFailure (show r)

    it "list with literals" $ do
      r <- either fail pure $ parseOne "(add 1 2)"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SAtom "ADD"),
                     Loc.Located _ (SExpr.SInt 1),
                     Loc.Located _ (SExpr.SInt 2)] -> pure ()
        _ -> expectationFailure (show r)

  describe "type annotations" $ do
    it "%INT" $ do
      r <- either fail pure $ parseOne "%INT"
      case r of
        SExpr.SType (Loc.Located _ (SExpr.SAtom "INT")) -> pure ()
        _ -> expectationFailure (show r)

    it "%(List %INT)" $ do
      r <- either fail pure $ parseOne "%(List %INT)"
      case r of
        SExpr.SType (Loc.Located _ (SExpr.SList
          [Loc.Located _ (SExpr.SAtom "LIST"),
           Loc.Located _ (SExpr.SType (Loc.Located _ (SExpr.SAtom "INT")))])) -> pure ()
        _ -> expectationFailure (show r)

  describe "quasiquote" $ do
    it "quasiquote atom" $ do
      r <- either fail pure $ parseOne "`foo"
      case r of
        SExpr.SQuasi (Loc.Located _ (SExpr.SAtom "FOO")) -> pure ()
        _ -> expectationFailure (show r)

    it "quasiquote list" $ do
      r <- either fail pure $ parseOne "`(if a b)"
      case r of
        SExpr.SQuasi (Loc.Located _ (SExpr.SList
          [Loc.Located _ (SExpr.SAtom "IF"),
           Loc.Located _ (SExpr.SAtom "A"),
           Loc.Located _ (SExpr.SAtom "B")])) -> pure ()
        _ -> expectationFailure (show r)

    it "unquote" $ do
      r <- either fail pure $ parseOne "`(if ,test b)"
      case r of
        SExpr.SQuasi (Loc.Located _ (SExpr.SList
          [Loc.Located _ (SExpr.SAtom "IF"),
           Loc.Located _ (SExpr.SUnquote (Loc.Located _ (SExpr.SAtom "TEST"))),
           Loc.Located _ (SExpr.SAtom "B")])) -> pure ()
        _ -> expectationFailure (show r)

    it "splice" $ do
      r <- either fail pure $ parseOne "`(do ,@body)"
      case r of
        SExpr.SQuasi (Loc.Located _ (SExpr.SList
          [Loc.Located _ (SExpr.SAtom "DO"),
           Loc.Located _ (SExpr.SSplice (Loc.Located _ (SExpr.SAtom "BODY")))])) -> pure ()
        _ -> expectationFailure (show r)

  describe "comments and whitespace" $ do
    it "comment stripped" $ do
      parseS "# comment\n42" `shouldBe` Right [SExpr.SInt 42]

    it "empty program" $ do
      parseS "" `shouldBe` Right []

    it "multiple top-level" $ do
      r <- either fail pure $ parseS "1 2 3"
      length r `shouldBe` 3

  describe "full program forms" $ do
    it "lambda form" $ do
      r <- either fail pure $ parseOne "(lam (x) x)"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SAtom "LAM"),
                     Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom "X")]),
                     Loc.Located _ (SExpr.SAtom "X")] -> pure ()
        _ -> expectationFailure (show r)

    it "lambda with types" $ do
      r <- either fail pure $ parseOne "(lam ((x %INT)) %INT x)"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SAtom "LAM"),
                     Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SList
                       [Loc.Located _ (SExpr.SAtom "X"),
                        Loc.Located _ (SExpr.SType (Loc.Located _ (SExpr.SAtom "INT")))])]),
                     Loc.Located _ (SExpr.SType (Loc.Located _ (SExpr.SAtom "INT"))),
                     Loc.Located _ (SExpr.SAtom "X")] -> pure ()
        _ -> expectationFailure (show r)

    it "macro definition form" $ do
      r <- either fail pure $ parseOne "(mac when (test body) `(if ,test ,body unit))"
      case r of
        SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : _) -> pure ()
        _ -> expectationFailure (show r)

  describe "SExpr → CST conversion" $ do
    it "integer literal" $ do
      prog <- viaSExpr "42"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLit (CST.LitInt 42))] -> pure ()
        other -> expectationFailure (show other)

    it "float literal" $ do
      prog <- viaSExpr "3.14"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLit (CST.LitFlt _))] -> pure ()
        other -> expectationFailure (show other)

    it "string literal" $ do
      prog <- viaSExpr "\"hello\""
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLit (CST.LitStr "hello"))] -> pure ()
        other -> expectationFailure (show other)

    it "bool true" $ do
      prog <- viaSExpr "true"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprBool True)] -> pure ()
        other -> expectationFailure (show other)

    it "bool false" $ do
      prog <- viaSExpr "false"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprBool False)] -> pure ()
        other -> expectationFailure (show other)

    it "unit" $ do
      prog <- viaSExpr "unit"
      case CST.progExprs prog of
        [Loc.Located _ CST.ExprUnit] -> pure ()
        other -> expectationFailure (show other)

    it "symbol" $ do
      prog <- viaSExpr "x"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprSym "X")] -> pure ()
        other -> expectationFailure (show other)

    it "application" $ do
      prog <- viaSExpr "(add 1 2)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprApp (Loc.Located _ (CST.ExprSym "ADD"))
                        [Loc.Located _ (CST.ExprLit (CST.LitInt 1)),
                         Loc.Located _ (CST.ExprLit (CST.LitInt 2))])] -> pure ()
        other -> expectationFailure (show other)

    it "lambda no type" $ do
      prog <- viaSExpr "(lam (x) x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam [CST.TSymbol "X" Nothing] Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "lambda with types" $ do
      prog <- viaSExpr "(lam ((x %INT)) %INT x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam [CST.TSymbol "X" (Just Ty.TyInt)] (Just Ty.TyInt) _)] -> pure ()
        other -> expectationFailure (show other)

    it "let" $ do
      prog <- viaSExpr "(let ((x 1)) x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLet [(CST.TSymbol "X" Nothing, _)] _)] -> pure ()
        other -> expectationFailure (show other)

    it "if" $ do
      prog <- viaSExpr "(if true 1 2)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprIf (Loc.Located _ (CST.ExprBool True)) _ _)] -> pure ()
        other -> expectationFailure (show other)

    it "type declaration" $ do
      prog <- viaSExpr "(type Maybe (a) (Nothing) (Just a))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprType "MAYBE" ["A"]
          [CST.DataCon "NOTHING" [] Nothing,
           CST.DataCon "JUST" [Ty.TyCon "A" []] Nothing])] -> pure ()
        other -> expectationFailure (show other)

    it "case" $ do
      prog <- viaSExpr "(case x (_ 1))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCase _ [(CST.PatWild, _)])] -> pure ()
        other -> expectationFailure (show other)

    it "case with patterns" $ do
      prog <- viaSExpr "(case x (true 1) (false 2) (v v))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCase _
          [(CST.PatBool True, _),
           (CST.PatBool False, _),
           (CST.PatVar "V", _)])] -> pure ()
        other -> expectationFailure (show other)

    it "case constructor pattern" $ do
      prog <- viaSExpr "(case x ((Just y) y))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCase _
          [(CST.PatCon "JUST" [CST.PatVar "Y"], _)])] -> pure ()
        other -> expectationFailure (show other)

    it "module declaration" $ do
      prog <- viaSExpr "(module Foo)"
      CST.progName prog `shouldBe` Just "FOO"

    it "import" $ do
      prog <- viaSExpr "(import Foo (bar baz))"
      length (CST.progImports prog) `shouldBe` 1
      let imp = head (CST.progImports prog)
      CST.impModule imp `shouldBe` "FOO"
      CST.impUnqual imp `shouldBe` ["BAR", "BAZ"]

    it "full program" $ do
      prog <- viaSExpr "(module Main) (import Foo) 42"
      CST.progName prog `shouldBe` Just "MAIN"
      length (CST.progImports prog) `shouldBe` 1
      length (CST.progExprs prog) `shouldBe` 1

    it "rx literal" $ do
      prog <- viaSExpr "/foo/gi"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLit (CST.LitRx "foo" "gi"))] -> pure ()
        other -> expectationFailure (show other)

    it "parameterized type annotation" $ do
      prog <- viaSExpr "(lam ((x %(List %INT))) x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam [CST.TSymbol "X" (Just (Ty.TyCon "LIST" [Ty.TyInt]))] Nothing _)] -> pure ()
        other -> expectationFailure (show other)

  describe "record types" $ do
    it "record type declaration" $ do
      prog <- viaSExpr "(type Person () (Person (name %STR) (age %INT)))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprType "PERSON" []
          [CST.DataCon "PERSON" [Ty.TyStr, Ty.TyInt] (Just ["NAME", "AGE"])])] -> pure ()
        other -> expectationFailure (show other)

    it "positional constructor unchanged" $ do
      prog <- viaSExpr "(type Maybe (a) (Nothing) (Just a))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprType "MAYBE" ["A"]
          [CST.DataCon "NOTHING" [] Nothing,
           CST.DataCon "JUST" [Ty.TyCon "A" []] Nothing])] -> pure ()
        other -> expectationFailure (show other)

    it "field access parses" $ do
      prog <- viaSExpr "(.name x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprFieldAccess "NAME" _)] -> pure ()
        other -> expectationFailure (show other)

-- Parse via SExpr pipeline: Text → [SExpr] → CST.Program
viaSExpr :: T.Text -> IO CST.Program
viaSExpr src = do
  sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>" src
  either (fail . show) pure $ SExpr.toProgram sexprs

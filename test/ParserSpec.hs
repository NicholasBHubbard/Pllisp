{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

import qualified Data.Text as T

import qualified Pllisp.CST    as CST
import qualified Pllisp.Parser as Parser
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type   as Ty

-- Parse a single expression from source
parseOne :: T.Text -> Either String CST.ExprF
parseOne src = case Parser.parseProgram "<test>" src of
  Left err -> Left (show err)
  Right prog -> case CST.progExprs prog of
    []                  -> Left "no expressions"
    (Loc.Located _ e:_) -> Right e

-- Parse whole program
parseAll :: T.Text -> Either String [CST.ExprF]
parseAll src = case Parser.parseProgram "<test>" src of
  Left err -> Left (show err)
  Right prog -> Right (map Loc.locVal (CST.progExprs prog))

spec :: Spec
spec = do
  describe "valid expressions" $ do
    it "lambda no type" $ do
      r <- either fail pure $ parseOne "(lam (x) x)"
      case r of
        CST.ExprLam [CST.TSymbol "X" Nothing] Nothing _ -> pure ()
        _ -> expectationFailure (show r)

    it "lambda with param type and return type" $ do
      r <- either fail pure $ parseOne "(lam ((x %INT)) %INT x)"
      case r of
        CST.ExprLam [CST.TSymbol "X" (Just Ty.TyInt)] (Just Ty.TyInt) _ -> pure ()
        _ -> expectationFailure (show r)

    it "application" $ do
      r <- either fail pure $ parseOne "(add 1 2)"
      case r of
        CST.ExprApp (Loc.Located _ (CST.ExprSym "ADD"))
                    [Loc.Located _ (CST.ExprLit (CST.LitInt 1)),
                     Loc.Located _ (CST.ExprLit (CST.LitInt 2))] -> pure ()
        _ -> expectationFailure (show r)

    it "let with empty bindings" $ do
      r <- either fail pure $ parseOne "(let () 42)"
      case r of
        CST.ExprLet [] _ -> pure ()
        _ -> expectationFailure (show r)

    it "let with binding" $ do
      r <- either fail pure $ parseOne "(let ((x 1)) x)"
      case r of
        CST.ExprLet [(CST.TSymbol "X" Nothing, _)] _ -> pure ()
        _ -> expectationFailure (show r)

    it "if expression" $ do
      r <- either fail pure $ parseOne "(if true 1 2)"
      case r of
        CST.ExprIf (Loc.Located _ (CST.ExprBool True)) _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "type declaration no params" $ do
      r <- either fail pure $ parseOne "(type Foo () (Bar))"
      case r of
        CST.ExprType "FOO" [] [CST.DataCon "BAR" []] -> pure ()
        _ -> expectationFailure (show r)

    it "type declaration with params and constructors" $ do
      r <- either fail pure $ parseOne "(type Maybe (a) (Nothing) (Just a))"
      case r of
        CST.ExprType "MAYBE" ["A"]
          [ CST.DataCon "NOTHING" []
          , CST.DataCon "JUST" [Ty.TyCon "A" []]
          ] -> pure ()
        _ -> expectationFailure (show r)

    it "case zero arms" $ do
      r <- either fail pure $ parseOne "(case x)"
      case r of
        CST.ExprCase _ [] -> pure ()
        _ -> expectationFailure (show r)

    it "case with one arm" $ do
      r <- either fail pure $ parseOne "(case x (_ 1))"
      case r of
        CST.ExprCase _ [(CST.PatWild, _)] -> pure ()
        _ -> expectationFailure (show r)

    it "bool true" $ do
      r <- either fail pure $ parseOne "true"
      r `shouldBe` CST.ExprBool True

    it "bool false" $ do
      r <- either fail pure $ parseOne "false"
      r `shouldBe` CST.ExprBool False

    it "unit" $ do
      r <- either fail pure $ parseOne "unit"
      r `shouldBe` CST.ExprUnit

    it "int literal" $ do
      r <- either fail pure $ parseOne "42"
      r `shouldBe` CST.ExprLit (CST.LitInt 42)

    it "float literal" $ do
      r <- either fail pure $ parseOne "3.14"
      r `shouldBe` CST.ExprLit (CST.LitFlt 3.14)

    it "string literal" $ do
      r <- either fail pure $ parseOne "\"hello\""
      r `shouldBe` CST.ExprLit (CST.LitStr "hello")

    it "symbol uppercased" $ do
      r <- either fail pure $ parseOne "x"
      r `shouldBe` CST.ExprSym "X"

    it "comment is stripped" $ do
      r <- either fail pure $ parseAll "# comment\n42"
      r `shouldBe` [CST.ExprLit (CST.LitInt 42)]

    it "empty program" $ do
      r <- either fail pure $ parseAll ""
      r `shouldBe` []

    it "multiple top-level expressions" $ do
      r <- either fail pure $ parseAll "1 2"
      length r `shouldBe` 2

  describe "pattern parsing" $ do
    it "PatWild" $ do
      r <- either fail pure $ parseOne "(case x (_ 0))"
      case r of
        CST.ExprCase _ [(CST.PatWild, _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatVar" $ do
      r <- either fail pure $ parseOne "(case x (v 0))"
      case r of
        CST.ExprCase _ [(CST.PatVar "V", _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatBool true" $ do
      r <- either fail pure $ parseOne "(case x (true 0))"
      case r of
        CST.ExprCase _ [(CST.PatBool True, _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatBool false" $ do
      r <- either fail pure $ parseOne "(case x (false 0))"
      case r of
        CST.ExprCase _ [(CST.PatBool False, _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatLit int" $ do
      r <- either fail pure $ parseOne "(case x (42 0))"
      case r of
        CST.ExprCase _ [(CST.PatLit (CST.LitInt 42), _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatLit string" $ do
      r <- either fail pure $ parseOne "(case x (\"hi\" 0))"
      case r of
        CST.ExprCase _ [(CST.PatLit (CST.LitStr "hi"), _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatCon zero args" $ do
      r <- either fail pure $ parseOne "(case x ((Nothing) 0))"
      case r of
        CST.ExprCase _ [(CST.PatCon "NOTHING" [], _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatCon with arg" $ do
      r <- either fail pure $ parseOne "(case x ((Just y) y))"
      case r of
        CST.ExprCase _ [(CST.PatCon "JUST" [CST.PatVar "Y"], _)] -> pure ()
        _ -> expectationFailure (show r)

    it "PatCon nested" $ do
      r <- either fail pure $ parseOne "(case x ((Just (Just y)) y))"
      case r of
        CST.ExprCase _ [(CST.PatCon "JUST" [CST.PatCon "JUST" [CST.PatVar "Y"]], _)] -> pure ()
        _ -> expectationFailure (show r)

  describe "type annotation parsing" $ do
    it "%INT in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %INT)) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just Ty.TyInt)] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "%FLT in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %FLT)) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just Ty.TyFlt)] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "%STR in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %STR)) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just Ty.TyStr)] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "%BOOL in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %BOOL)) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just Ty.TyBool)] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "%UNIT in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %UNIT)) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just Ty.TyUnit)] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "%UNIT as return type" $ do
      r <- either fail pure $ parseOne "(lam (x) %UNIT x)"
      case r of
        CST.ExprLam _ (Just Ty.TyUnit) _ -> pure ()
        _ -> expectationFailure (show r)

    it "%Maybe (bare TyCon) in lambda return type" $ do
      r <- either fail pure $ parseOne "(lam (x) %Maybe x)"
      case r of
        CST.ExprLam _ (Just (Ty.TyCon "MAYBE" [])) _ -> pure ()
        _ -> expectationFailure (show r)

    it "%(List %INT) parameterized type in lambda param" $ do
      r <- either fail pure $ parseOne "(lam ((x %(List %INT))) x)"
      case r of
        CST.ExprLam [CST.TSymbol _ (Just (Ty.TyCon "LIST" [Ty.TyInt]))] _ _ -> pure ()
        _ -> expectationFailure (show r)

    it "type var in constructor arg" $ do
      r <- either fail pure $ parseOne "(type List (a) (Cons a %INT))"
      case r of
        CST.ExprType "LIST" ["A"] [CST.DataCon "CONS" [Ty.TyCon "A" [], Ty.TyInt]] -> pure ()
        _ -> expectationFailure (show r)

  describe "module declaration" $ do
    it "parses module name" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>" "(module Foo)"
      CST.progName prog `shouldBe` Just "FOO"
      CST.progExprs prog `shouldBe` []

    it "no module declaration gives Nothing" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>" "42"
      CST.progName prog `shouldBe` Nothing

    it "module keyword is reserved" $ do
      case Parser.parseProgram "<test>" "module" of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected parse error"

  describe "import declaration" $ do
    it "parses qualified import" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>" "(import Foo)"
      length (CST.progImports prog) `shouldBe` 1
      let imp = head (CST.progImports prog)
      CST.impModule imp `shouldBe` "FOO"
      CST.impUnqual imp `shouldBe` []

    it "parses import with unqualified symbols" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>" "(import Foo (bar baz))"
      let imp = head (CST.progImports prog)
      CST.impModule imp `shouldBe` "FOO"
      CST.impUnqual imp `shouldBe` ["BAR", "BAZ"]

    it "parses multiple imports" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>" "(import A) (import B (x))"
      length (CST.progImports prog) `shouldBe` 2

    it "import keyword is reserved" $ do
      case Parser.parseProgram "<test>" "import" of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected parse error"

  describe "dotted symbols" $ do
    it "parses Foo.bar as qualified symbol" $ do
      r <- either fail pure $ parseOne "Foo.bar"
      r `shouldBe` CST.ExprSym "FOO.BAR"

    it "plain symbol still works" $ do
      r <- either fail pure $ parseOne "x"
      r `shouldBe` CST.ExprSym "X"

  describe "full program" $ do
    it "parses module + imports + exprs" $ do
      prog <- either (fail . show) pure $ Parser.parseProgram "<test>"
        "(module Main) (import Foo) (import Bar (x)) 42"
      CST.progName prog `shouldBe` Just "MAIN"
      length (CST.progImports prog) `shouldBe` 2
      length (CST.progExprs prog) `shouldBe` 1

  describe "rx literal" $ do
    it "simple rx" $ do
      r <- either fail pure $ parseOne "/foo/"
      r `shouldBe` CST.ExprLit (CST.LitRx "foo" "")

    it "rx with flags" $ do
      r <- either fail pure $ parseOne "/foo/gi"
      r `shouldBe` CST.ExprLit (CST.LitRx "foo" "gi")

    it "rx with backslash sequences" $ do
      r <- either fail pure $ parseOne "/\\d+/"
      r `shouldBe` CST.ExprLit (CST.LitRx "\\d+" "")

    it "rx with escaped slash" $ do
      r <- either fail pure $ parseOne "/foo\\/bar/"
      r `shouldBe` CST.ExprLit (CST.LitRx "foo/bar" "")

    it "empty rx" $ do
      r <- either fail pure $ parseOne "//"
      r `shouldBe` CST.ExprLit (CST.LitRx "" "")

    it "rx with all flag types" $ do
      r <- either fail pure $ parseOne "/test/imsx"
      r `shouldBe` CST.ExprLit (CST.LitRx "test" "imsx")

    it "rx with \\Q\\E quoting" $ do
      r <- either fail pure $ parseOne "/\\Qfoo.bar\\E/"
      r `shouldBe` CST.ExprLit (CST.LitRx "\\Qfoo.bar\\E" "")

    it "rx as function argument" $ do
      r <- either fail pure $ parseOne "(rx-match /hello/ \"world\")"
      case r of
        CST.ExprApp _ [Loc.Located _ (CST.ExprLit (CST.LitRx "hello" "")), _] -> pure ()
        _ -> expectationFailure (show r)

    it "%RX type annotation" $ do
      r <- either fail pure $ parseOne "(lam ((r %RX)) r)"
      case r of
        CST.ExprLam [CST.TSymbol "R" (Just Ty.TyRx)] _ _ -> pure ()
        _ -> expectationFailure (show r)

  describe "error cases" $ do
    it "unclosed paren" $ do
      case Parser.parseProgram "<test>" "(add 1 2" of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected parse error"

    it "missing body in let" $ do
      case Parser.parseProgram "<test>" "(let ((x 1)))" of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected parse error"

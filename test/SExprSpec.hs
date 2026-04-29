{-# LANGUAGE OverloadedStrings #-}

module SExprSpec (spec) where

import Test.Hspec

import Data.Either (isLeft)
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
  SExpr.SUSym _  -> f
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

    it "%SYNTAX" $ do
      r <- either fail pure $ parseOne "%SYNTAX"
      case r of
        SExpr.SType (Loc.Located _ (SExpr.SAtom "SYNTAX")) -> pure ()
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
        [Loc.Located _ (CST.ExprLam (CST.LamList [CST.TSymbol "X" Nothing] CST.NoExtra) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "lambda with types" $ do
      prog <- viaSExpr "(lam ((x %INT)) %INT x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList [CST.TSymbol "X" (Just Ty.TyInt)] CST.NoExtra) (Just Ty.TyInt) _)] -> pure ()
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
      CST.impAlias imp `shouldBe` "FOO"
      CST.impUnqual imp `shouldBe` ["BAR", "BAZ"]

    it "import with alias" $ do
      prog <- viaSExpr "(import Foo F (bar))"
      let imp = head (CST.progImports prog)
      CST.impModule imp `shouldBe` "FOO"
      CST.impAlias imp `shouldBe` "F"
      CST.impUnqual imp `shouldBe` ["BAR"]

    it "import with alias only" $ do
      prog <- viaSExpr "(import Foo F)"
      let imp = head (CST.progImports prog)
      CST.impModule imp `shouldBe` "FOO"
      CST.impAlias imp `shouldBe` "F"
      CST.impUnqual imp `shouldBe` []

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
        [Loc.Located _ (CST.ExprLam (CST.LamList [CST.TSymbol "X" (Just (Ty.TyCon "LIST" [Ty.TyInt]))] CST.NoExtra) Nothing _)] -> pure ()
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

  describe "extended lambda lists" $ do
    -- &rest
    it "&rest with required params" $ do
      prog <- viaSExpr "(lam (a b &rest xs) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing, CST.TSymbol "B" Nothing]
          (CST.RestParam (CST.TSymbol "XS" Nothing))) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "&rest no required params" $ do
      prog <- viaSExpr "(lam (&rest xs) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList []
          (CST.RestParam (CST.TSymbol "XS" Nothing))) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "&rest with typed param" $ do
      prog <- viaSExpr "(lam (a &rest (xs %(List %INT))) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing]
          (CST.RestParam (CST.TSymbol "XS" (Just (Ty.TyCon "LIST" [Ty.TyInt]))))) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    -- %opt
    it "%opt single param" $ do
      prog <- viaSExpr "(lam (a %opt (b 0)) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing]
          (CST.OptParams [(CST.TSymbol "B" Nothing, Loc.Located _ (CST.ExprLit (CST.LitInt 0)))])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "%opt multiple params" $ do
      prog <- viaSExpr "(lam (a %opt (b 0) (c \"hi\")) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing]
          (CST.OptParams [
            (CST.TSymbol "B" Nothing, Loc.Located _ (CST.ExprLit (CST.LitInt 0))),
            (CST.TSymbol "C" Nothing, Loc.Located _ (CST.ExprLit (CST.LitStr "hi")))])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "%opt no required params" $ do
      prog <- viaSExpr "(lam (%opt (b 0)) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList []
          (CST.OptParams [(CST.TSymbol "B" Nothing, _)])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "%opt with typed param" $ do
      prog <- viaSExpr "(lam (%opt ((b %INT) 0)) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList []
          (CST.OptParams [(CST.TSymbol "B" (Just Ty.TyInt), _)])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    -- &key
    it "&key params" $ do
      prog <- viaSExpr "(lam (&key (x 0) (y 1)) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList []
          (CST.KeyParams [
            (CST.TSymbol "X" Nothing, Loc.Located _ (CST.ExprLit (CST.LitInt 0))),
            (CST.TSymbol "Y" Nothing, Loc.Located _ (CST.ExprLit (CST.LitInt 1)))])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "&key with required params" $ do
      prog <- viaSExpr "(lam (a &key (x 0)) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing]
          (CST.KeyParams [(CST.TSymbol "X" Nothing, _)])) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    -- &key at call sites
    it "&key in application" $ do
      prog <- viaSExpr "(f &key x 1 &key y 2)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprApp _ [
          Loc.Located _ (CST.ExprKeyArg "X" (Loc.Located _ (CST.ExprLit (CST.LitInt 1)))),
          Loc.Located _ (CST.ExprKeyArg "Y" (Loc.Located _ (CST.ExprLit (CST.LitInt 2))))])] -> pure ()
        other -> expectationFailure (show other)

    it "&key mixed with positional args" $ do
      prog <- viaSExpr "(f 1 &key x 2)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprApp _ [
          Loc.Located _ (CST.ExprLit (CST.LitInt 1)),
          Loc.Located _ (CST.ExprKeyArg "X" (Loc.Located _ (CST.ExprLit (CST.LitInt 2))))])] -> pure ()
        other -> expectationFailure (show other)

    -- plain lambda unchanged
    it "plain lambda uses NoExtra" $ do
      prog <- viaSExpr "(lam (a b) 1)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList
          [CST.TSymbol "A" Nothing, CST.TSymbol "B" Nothing]
          CST.NoExtra) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

  describe "extended lambda list errors" $ do
    it "rejects &rest with no param after" $
      shouldFailSExpr "(lam (&rest) 1)"

    it "rejects &rest with two params after" $
      shouldFailSExpr "(lam (&rest a b) 1)"

    it "rejects &rest followed by %opt" $
      shouldFailSExpr "(lam (&rest %opt (x 0)) 1)"

    it "rejects %opt followed by &rest" $
      shouldFailSExpr "(lam (%opt (x 0) &rest y) 1)"

    it "rejects &rest followed by &key" $
      shouldFailSExpr "(lam (&rest a &key (x 0)) 1)"

    it "rejects &key followed by &rest" $
      shouldFailSExpr "(lam (&key (x 0) &rest y) 1)"

    it "rejects %opt followed by &key" $
      shouldFailSExpr "(lam (%opt (x 0) &key (y 1)) 1)"

    it "rejects &key followed by %opt" $
      shouldFailSExpr "(lam (&key (x 0) %opt (y 1)) 1)"

    it "rejects %opt with bare symbol (no default)" $
      shouldFailSExpr "(lam (%opt b) 1)"

    it "rejects &key with bare symbol (no default)" $
      shouldFailSExpr "(lam (&key b) 1)"

    it "rejects duplicate &rest" $
      shouldFailSExpr "(lam (&rest a &rest b) 1)"

    it "rejects &rest param written as default pair" $
      shouldFailSExpr "(lam (&rest (a 0)) 1)"

  describe "reserved names" $ do
    it "rejects reserved word in let binding" $ do
      let sexprs = case Parser.parseSExprs "<test>" "(let ((if 1)) if)" of
            Left _  -> error "parse should succeed"
            Right s -> s
      case SExpr.toProgram sexprs of
        Left err  -> SExpr.ceMsg err `shouldContain` "reserved word cannot be used as binding name: IF"
        Right _ -> expectationFailure "expected reserved-word binding error"

  describe "typeclasses" $ do
    -- cls
    it "cls with one method" $ do
      prog <- viaSExpr "(cls SHOW () (a) (show %a %STR))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCls "SHOW" ["A"] []
          [CST.ClassMethod "SHOW" [Ty.TyCon "A" []] Ty.TyStr])] -> pure ()
        other -> expectationFailure (show other)

    it "cls with multi-arg method" $ do
      prog <- viaSExpr "(cls EQUAL () (a) (equal %a %a %BOOL))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCls "EQUAL" ["A"] []
          [CST.ClassMethod "EQUAL" [Ty.TyCon "A" [], Ty.TyCon "A" []] Ty.TyBool])] -> pure ()
        other -> expectationFailure (show other)

    it "cls with multiple methods" $ do
      prog <- viaSExpr "(cls NUM () (a) (add %a %a %a) (neg %a %a))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCls "NUM" ["A"] []
          [CST.ClassMethod "ADD" [Ty.TyCon "A" [], Ty.TyCon "A" []] (Ty.TyCon "A" []),
           CST.ClassMethod "NEG" [Ty.TyCon "A" []] (Ty.TyCon "A" [])])] -> pure ()
        other -> expectationFailure (show other)

    it "cls with multiple type vars" $ do
      prog <- viaSExpr "(cls CONVERT () (a b) (convert %a %b))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCls "CONVERT" ["A", "B"] []
          [CST.ClassMethod "CONVERT" [Ty.TyCon "A" []] (Ty.TyCon "B" [])])] -> pure ()
        other -> expectationFailure (show other)

    it "cls with superclasses" $ do
      prog <- viaSExpr "(cls APPLICATIVE (FUNCTOR) (f) (pure %a %(f a)))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCls "APPLICATIVE" ["F"] ["FUNCTOR"]
          [CST.ClassMethod "PURE" [Ty.TyCon "A" []] (Ty.TyCon "F" [Ty.TyCon "A" []])])] -> pure ()
        other -> expectationFailure (show other)

    -- inst
    it "inst with one method" $ do
      prog <- viaSExpr "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprInst "SHOW" Ty.TyInt
          [("SHOW", Loc.Located _ (CST.ExprLam _ _ _))])] -> pure ()
        other -> expectationFailure (show other)

    it "inst with multiple methods" $ do
      prog <- viaSExpr "(inst NUM %INT (add (lam ((x %INT) (y %INT)) x)) (neg (lam ((x %INT)) x)))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprInst "NUM" Ty.TyInt
          [("ADD", _), ("NEG", _)])] -> pure ()
        other -> expectationFailure (show other)

    it "inst for parameterized type" $ do
      prog <- viaSExpr "(inst SHOW %(List %INT) (show (lam ((xs %(List %INT))) \"list\")))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprInst "SHOW" (Ty.TyCon "LIST" [Ty.TyInt])
          [("SHOW", _)])] -> pure ()
        other -> expectationFailure (show other)

    -- errors
    it "rejects cls with no type vars" $
      shouldFailSExpr "(cls SHOW () (show %a %STR))"

    it "rejects cls with no methods" $
      shouldFailSExpr "(cls SHOW (a))"

    it "rejects cls method with only one type (no args)" $
      shouldFailSExpr "(cls SHOW (a) (show %STR))"

    it "rejects inst with no methods" $
      shouldFailSExpr "(inst SHOW %INT)"

  describe "preScanImports" $ do
    it "extracts import from raw sexprs" $ do
      sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>" "(import Foo)"
      let imps = SExpr.preScanImports sexprs
      length imps `shouldBe` 1
      CST.impModule (head imps) `shouldBe` "FOO"

    it "extracts import with unqualified names" $ do
      sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>" "(import Bar (x y))"
      let imps = SExpr.preScanImports sexprs
      length imps `shouldBe` 1
      CST.impModule (head imps) `shouldBe` "BAR"
      CST.impUnqual (head imps) `shouldBe` ["X", "Y"]

    it "finds imports mixed with other forms" $ do
      sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>"
        "(mac foo () `1) (import A) (let ((x 1)) x) (import B (y))"
      let imps = SExpr.preScanImports sexprs
      length imps `shouldBe` 2
      CST.impModule (head imps) `shouldBe` "A"
      CST.impModule (imps !! 1) `shouldBe` "B"

    it "returns empty when no imports" $ do
      sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>" "42 (let ((x 1)) x)"
      SExpr.preScanImports sexprs `shouldBe` []

    it "skips module declaration" $ do
      sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>"
        "(module Foo) (import Bar)"
      let imps = SExpr.preScanImports sexprs
      length imps `shouldBe` 1
      CST.impModule (head imps) `shouldBe` "BAR"

  describe "uninterned symbols" $ do
    it "parses :foo as SUSym" $ do
      parseOne ":foo" `shouldBe` Right (SExpr.SUSym "FOO")

    it "parses :verbose as SUSym" $ do
      parseOne ":verbose" `shouldBe` Right (SExpr.SUSym "VERBOSE")

    it "case insensitive" $ do
      parseOne ":FOO" `shouldBe` Right (SExpr.SUSym "FOO")
      parseOne ":Foo" `shouldBe` Right (SExpr.SUSym "FOO")

    it "usym inside list" $ do
      r <- either fail pure $ parseOne "(:foo 42)"
      case r of
        SExpr.SList [Loc.Located _ (SExpr.SUSym "FOO"),
                     Loc.Located _ (SExpr.SInt 42)] -> pure ()
        _ -> expectationFailure (show r)

    it "usym converts to CST literal" $ do
      prog <- viaSExpr ":foo"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLit (CST.LitUSym "FOO"))] -> pure ()
        other -> expectationFailure (show other)

    it "usym as pattern" $ do
      prog <- viaSExpr "(case x (:foo 1) (_ 2))"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprCase _
          [(CST.PatLit (CST.LitUSym "FOO"), _),
           (CST.PatWild, _)])] -> pure ()
        other -> expectationFailure (show other)

    it "%USYM type annotation" $ do
      prog <- viaSExpr "(lam ((x %USYM)) x)"
      case CST.progExprs prog of
        [Loc.Located _ (CST.ExprLam (CST.LamList [CST.TSymbol "X" (Just Ty.TyUSym)] CST.NoExtra) Nothing _)] -> pure ()
        other -> expectationFailure (show other)

    it "colon alone does not parse as usym" $ do
      parseOne ":" `shouldSatisfy` isLeft

    it "colon followed by digit does not parse as usym" $ do
      parseOne ":123" `shouldSatisfy` isLeft

  describe "toProgram with interleaved forms" $ do
    it "finds imports after type definitions" $ do
      prog <- viaSExpr "(type Foo () (Bar)) (import Mod) 42"
      length (CST.progImports prog) `shouldBe` 1
      CST.impModule (head (CST.progImports prog)) `shouldBe` "MOD"

    it "finds module declaration after type definitions" $ do
      sexprs <- either (fail . show) pure $
        Parser.parseSExprs "<test>" "(type Foo () (Bar)) (module Main) 42"
      prog <- either (fail . show) pure $ SExpr.toProgram sexprs
      CST.progName prog `shouldBe` Just "MAIN"

    it "finds both module and imports after type defs" $ do
      sexprs <- either (fail . show) pure $
        Parser.parseSExprs "<test>" "(type Foo () (Bar)) (module Main) (import Lib (x)) 42"
      prog <- either (fail . show) pure $ SExpr.toProgram sexprs
      CST.progName prog `shouldBe` Just "MAIN"
      length (CST.progImports prog) `shouldBe` 1
      CST.impUnqual (head (CST.progImports prog)) `shouldBe` ["X"]

-- Parse via SExpr pipeline: Text → [SExpr] → CST.Program
viaSExpr :: T.Text -> IO CST.Program
viaSExpr src = do
  sexprs <- either (fail . show) pure $ Parser.parseSExprs "<test>" src
  either (fail . show) pure $ SExpr.toProgram sexprs

-- Expect SExpr → CST conversion to fail
shouldFailSExpr :: T.Text -> Expectation
shouldFailSExpr src = do
  let sexprs = case Parser.parseSExprs "<test>" src of
        Left _  -> error "parse should succeed"
        Right s -> s
  case SExpr.toProgram sexprs of
    Left _  -> pure ()
    Right _ -> expectationFailure "expected conversion error"

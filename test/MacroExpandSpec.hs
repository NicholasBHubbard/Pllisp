{-# LANGUAGE OverloadedStrings #-}

module MacroExpandSpec (spec) where

import Test.Hspec

import qualified Data.Text as T

import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.Parser      as Parser
import qualified Pllisp.SExpr       as SExpr
import qualified Pllisp.SrcLoc      as Loc

-- Parse source to SExprs (without expanding)
parseSexprs :: T.Text -> Either String [SExpr.SExpr]
parseSexprs src = case Parser.parseSExprs "<test>" src of
  Left err -> Left (show err)
  Right s  -> Right s

-- Parse source to SExprs, expand macros, return stripped results
expandSrc :: T.Text -> Either String [SExpr.SExprF]
expandSrc src = do
  sexprs <- case Parser.parseSExprs "<test>" src of
    Left err -> Left (show err)
    Right s  -> Right s
  expanded <- MacroExpand.expand sexprs
  Right (map strip expanded)

-- Strip all locations for comparison
strip :: SExpr.SExpr -> SExpr.SExprF
strip (Loc.Located _ f) = case f of
  SExpr.SList xs       -> SExpr.SList (map stripL xs)
  SExpr.SType inner    -> SExpr.SType (stripL inner)
  SExpr.SQuasi inner   -> SExpr.SQuasi (stripL inner)
  SExpr.SUnquote inner -> SExpr.SUnquote (stripL inner)
  SExpr.SSplice inner  -> SExpr.SSplice (stripL inner)
  other -> other

stripL :: SExpr.SExpr -> SExpr.SExpr
stripL sx = Loc.Located dummySpan (strip sx)

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)

-- Helper: build a located SExprF with dummy span
l :: SExpr.SExprF -> SExpr.SExpr
l = Loc.Located dummySpan

spec :: Spec
spec = do
  describe "macro definitions are removed" $ do
    it "removes mac form from output" $ do
      r <- either fail pure $ expandSrc "(mac foo () `unit) 42"
      r `shouldBe` [SExpr.SInt 42]

    it "multiple mac forms removed" $ do
      r <- either fail pure $ expandSrc "(mac a () `unit) (mac b () `unit) 42"
      r `shouldBe` [SExpr.SInt 42]

  describe "simple template expansion" $ do
    it "expands macro with no args" $ do
      r <- either fail pure $ expandSrc "(mac five () `5) (five)"
      r `shouldBe` [SExpr.SInt 5]

    it "expands macro with one arg" $ do
      r <- either fail pure $ expandSrc "(mac double (x) `(add ,x ,x)) (double 3)"
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "ADD"), l (SExpr.SInt 3), l (SExpr.SInt 3)]]

    it "expands when macro" $ do
      r <- either fail pure $ expandSrc
        "(mac when (test body) `(if ,test ,body unit)) (when true 42)"
      case r of
        [SExpr.SList [_, _, _, Loc.Located _ (SExpr.SAtom "UNIT")]] -> pure ()
        _ -> expectationFailure (show r)

  describe "multi-clause macros" $ do
    it "matches first clause" $ do
      r <- either fail pure $ expandSrc "(mac do (expr) expr) (do 42)"
      r `shouldBe` [SExpr.SInt 42]

    it "matches second clause with &rest" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac do (expr) expr)"
                   , "(mac do (first &rest rest) `(let ((_ ,first)) (do ,@rest)))"
                   , "(do 1 2 3)"
                   ])
      -- Should expand to nested lets: (let ((_ 1)) (let ((_ 2)) 3))
      case r of
        [SExpr.SList (Loc.Located _ (SExpr.SAtom "LET") : _)] -> pure ()
        _ -> expectationFailure (show r)

  describe "recursive expansion" $ do
    it "re-expands macro calls in expansion output" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac unless (test body) `(if ,test unit ,body))"
                   , "(mac when (test body) `(unless (not ,test) ,body))"
                   , "(when true 42)"
                   ])
      -- when → unless → if
      case r of
        [SExpr.SList (Loc.Located _ (SExpr.SAtom "IF") : _)] -> pure ()
        _ -> expectationFailure (show r)

  describe "splice ,@" $ do
    it "splices &rest args into list" $ do
      r <- either fail pure $ expandSrc "(mac my-list (&rest xs) `(list ,@xs)) (my-list 1 2 3)"
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "LIST"),
                                  l (SExpr.SInt 1),
                                  l (SExpr.SInt 2),
                                  l (SExpr.SInt 3)]]

  describe "non-macro code passes through" $ do
    it "regular expressions unchanged" $ do
      r <- either fail pure $ expandSrc "(add 1 2)"
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "ADD"), l (SExpr.SInt 1), l (SExpr.SInt 2)]]

    it "no macros means identity" $ do
      r <- either fail pure $ expandSrc "42 \"hello\" true"
      r `shouldBe` [SExpr.SInt 42, SExpr.SStr "hello", SExpr.SAtom "TRUE"]

  describe "extractMacroDefs" $ do
    it "extracts mac forms from mixed sexprs" $ do
      sexprs <- either (fail . show) pure $ parseSexprs
        "(mac foo () `1) 42 (mac bar (x) `,x) (let ((y 2)) y)"
      let macDefs = MacroExpand.extractMacroDefs sexprs
      length macDefs `shouldBe` 2

    it "returns empty when no mac forms" $ do
      sexprs <- either (fail . show) pure $ parseSexprs "42 (let ((x 1)) x)"
      MacroExpand.extractMacroDefs sexprs `shouldBe` []

    it "extracted mac defs are usable in expand" $ do
      sexprs <- either (fail . show) pure $ parseSexprs
        "(mac double (x) `(add ,x ,x)) 42 (type Foo () (Bar))"
      let macDefs = MacroExpand.extractMacroDefs sexprs
      -- Use extracted mac defs to expand a call
      callSexprs <- either (fail . show) pure $ parseSexprs "(double 3)"
      r <- either fail pure $ MacroExpand.expand (macDefs ++ callSexprs)
      map strip r `shouldBe`
        [SExpr.SList [l (SExpr.SAtom "ADD"), l (SExpr.SInt 3), l (SExpr.SInt 3)]]

  describe "error cases" $ do
    it "wrong number of args" $ do
      case expandSrc "(mac foo (a b) `(add ,a ,b)) (foo 1)" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error"

    it "expansion depth limit" $ do
      case expandSrc "(mac loop () `(loop)) (loop)" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected depth limit error"

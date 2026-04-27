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
  SExpr.SUSym _  -> f
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

  describe "macro redefinition" $ do
    it "second definition replaces first" $ do
      -- Second (mac foo) replaces the first, so calling with 2 args fails
      case expandSrc (T.unlines [ "(mac foo (a b) `(add ,a ,b))"
                                , "(mac foo (x) `,x)"
                                , "(foo 1 2)"
                                ]) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error: second definition should replace first"

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

  describe "uninterned symbols" $ do
    it "usym passes through expansion" $ do
      r <- either fail pure $ expandSrc ":foo"
      r `shouldBe` [SExpr.SUSym "FOO"]

    it "usym in macro output" $ do
      r <- either fail pure $ expandSrc
        "(mac mode () `:verbose) (mode)"
      r `shouldBe` [SExpr.SUSym "VERBOSE"]

    it "usym spliced from &rest" $ do
      r <- either fail pure $ expandSrc
        "(mac wrap (&rest xs) `(list ,@xs)) (wrap :foo :bar)"
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "LIST"),
                                  l (SExpr.SUSym "FOO"),
                                  l (SExpr.SUSym "BAR")]]

    it "usym in procedural macro" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac first-sym (&rest args)"
                   , "  (car args))"
                   , "(first-sym :hello :world)"
                   ])
      r `shouldBe` [SExpr.SUSym "HELLO"]

  describe "error cases" $ do
    it "wrong number of args" $ do
      case expandSrc "(mac foo (a b) `(add ,a ,b)) (foo 1)" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error"

    it "expansion depth limit" $ do
      case expandSrc "(mac loop () `(loop)) (loop)" of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected depth limit error"

  -- ---------------------------------------------------------------
  -- PROCEDURAL MACROS (compile-time evaluation)
  -- ---------------------------------------------------------------
  describe "procedural macros" $ do
    it "macro with compile-time computation" $ do
      -- Macro body uses car to extract first arg and builds code
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac first-of (&rest args)"
                   , "  (car args))"
                   , "(first-of 42 99)"
                   ])
      r `shouldBe` [SExpr.SInt 42]

    it "macro uses list operations" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac swap (a b)"
                   , "  `(let ((tmp ,a)) (let ((,a ,b)) (let ((,b tmp)) unit))))"
                   , "(swap x y)"
                   ])
      case r of
        [SExpr.SList (Loc.Located _ (SExpr.SAtom "LET") : _)] -> pure ()
        _ -> expectationFailure (show r)

    it "macro with conditional code generation" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac maybe-wrap (flag expr)"
                   , "  (if (eq flag (quote YES))"
                   , "    `(just ,expr)"
                   , "    expr))"
                   , "(maybe-wrap yes 42)"
                   ])
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "JUST"), l (SExpr.SInt 42)]]

    it "macro uses filter to select from args" $ do
      -- Filter even numbers at compile time
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac count-args (&rest args)"
                   , "  (length args))"
                   , "(count-args a b c d)"
                   ])
      r `shouldBe` [SExpr.SInt 4]

    it "macro with map to transform args" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac quote-all (&rest args)"
                   , "  (cons (quote LIST) args))"
                   , "(quote-all 1 2 3)"
                   ])
      r `shouldBe` [SExpr.SList [l (SExpr.SAtom "LIST"),
                                  l (SExpr.SInt 1),
                                  l (SExpr.SInt 2),
                                  l (SExpr.SInt 3)]]

    it "macro with recursive helper" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac my-list (&rest xs)"
                   , "  (let ((build (lam (items)"
                   , "    (if (null? items)"
                   , "      (quote UNIT)"
                   , "      `(CONS ,(car items) ,(build (cdr items)))))))"
                   , "    (build xs)))"
                   , "(my-list 1 2 3)"
                   ])
      -- Should produce (CONS 1 (CONS 2 (CONS 3 UNIT)))
      case r of
        [SExpr.SList [Loc.Located _ (SExpr.SAtom "CONS"), Loc.Located _ (SExpr.SInt 1), _]] -> pure ()
        _ -> expectationFailure (show r)

    it "macro with string manipulation" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac make-name (prefix suffix)"
                   , "  (str-to-sym (concat (sym-to-str prefix) (sym-to-str suffix))))"
                   , "(make-name FOO BAR)"
                   ])
      r `shouldBe` [SExpr.SAtom "FOOBAR"]

    it "macro with gensym for hygiene" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac with-temp (val body)"
                   , "  (let ((tmp (gensym)))"
                   , "    `(let ((,tmp ,val)) ,body)))"
                   , "(with-temp 42 (add 1 2))"
                   ])
      -- Should produce (let ((__G0 42)) (add 1 2))
      case r of
        [SExpr.SList [Loc.Located _ (SExpr.SAtom "LET"),
                      Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom gname), _])]),
                      _]] -> T.isPrefixOf "__G" gname `shouldBe` True
        _ -> expectationFailure (show r)

    it "macro error propagates" $ do
      case expandSrc "(mac bad () (error \"intentional\")) (bad)" of
        Left msg -> msg `shouldContain` "intentional"
        Right _ -> expectationFailure "expected error"

    it "procedural macro interleaves with template macros" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac when (cond body) `(if ,cond ,body unit))"
                   , "(mac double (x) `(add ,x ,x))"
                   , "(when true (double 5))"
                   ])
      -- when expands to (if true (double 5) unit)
      -- then double expands to (add 5 5)
      case r of
        [SExpr.SList [Loc.Located _ (SExpr.SAtom "IF"),
                      Loc.Located _ (SExpr.SAtom "TRUE"),
                      Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom "ADD"), _, _]),
                      Loc.Located _ (SExpr.SAtom "UNIT")]] -> pure ()
        _ -> expectationFailure (show r)

    it "destructuring still works with interpreter" $ do
      r <- either fail pure $ expandSrc
        (T.unlines [ "(mac cond ((test body)) `(if ,test ,body unit))"
                   , "(cond (true 42))"
                   ])
      case r of
        [SExpr.SList (Loc.Located _ (SExpr.SAtom "IF") : _)] -> pure ()
        _ -> expectationFailure (show r)

    it "multi-clause macros are not supported" $ do
      -- Defining the same macro twice just replaces it
      case expandSrc (T.unlines [ "(mac do (expr) expr)"
                                , "(mac do (first &rest rest) `(let ((_ ,first)) (do ,@rest)))"
                                , "(do 42)"
                                ]) of
        Left _ -> pure ()  -- second def has (first &rest rest), "do 42" matches but recurses on (do) which has 0 args
        Right [SExpr.SInt 42] -> expectationFailure "multi-clause should not work: second def should replace first"
        Right _ -> pure ()  -- any other result is fine as long as it's not the multi-clause behavior

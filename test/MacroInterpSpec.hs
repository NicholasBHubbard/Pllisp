{-# LANGUAGE OverloadedStrings #-}

module MacroInterpSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.MacroInterp as MI
import qualified Pllisp.Parser      as Parser
import qualified Pllisp.SExpr       as SExpr
import qualified Pllisp.SrcLoc      as Loc

-- Helpers

-- | Parse a single SExpr from source text.
parseSExpr :: T.Text -> SExpr.SExpr
parseSExpr src = case Parser.parseSExprs "<test>" src of
  Right [s] -> s
  Right ss  -> error $ "expected 1 sexpr, got " ++ show (length ss)
  Left err  -> error $ "parse error: " ++ show err

-- | Evaluate source text in the default environment.
evalSrc :: T.Text -> Either String MI.MVal
evalSrc src = MI.runInterpM (MI.eval (MacroExpand.csEnv MacroExpand.defaultState) (parseSExpr src))

-- | Evaluate source text in a custom environment (merged with defaults).
evalWith :: [(T.Text, MI.MVal)] -> T.Text -> Either String MI.MVal
evalWith binds src =
  let env = M.union (M.fromList binds) (MacroExpand.csEnv MacroExpand.defaultState)
  in MI.runInterpM (MI.eval env (parseSExpr src))

-- | Assert evaluation succeeds with expected value.
shouldEvalTo :: T.Text -> MI.MVal -> Expectation
shouldEvalTo src expected = case evalSrc src of
  Right val -> val `shouldBe` expected
  Left err  -> expectationFailure $ "eval failed: " ++ err

-- | Assert evaluation fails.
shouldFail :: T.Text -> Expectation
shouldFail src = case evalSrc src of
  Left _  -> pure ()
  Right v -> expectationFailure $ "expected error, got: " ++ show v

-- | Parse and expand macros (for integration edge cases).
expandSrc :: T.Text -> Either String [SExpr.SExpr]
expandSrc src = do
  sexprs <- case Parser.parseSExprs "<test>" src of
    Left err -> Left (show err)
    Right s  -> Right s
  MacroExpand.expand sexprs

spec :: Spec
spec = do
  describe "default environment" $ do
    it "includes higher-level list helpers from PRELUDE compile-time support" $ do
      let expectClosure name = case M.lookup name (MacroExpand.csEnv MacroExpand.defaultState) of
            Just MI.MClosure{} -> pure ()
            Just MI.MTypedClosure{} -> pure ()
            Just other -> expectationFailure ("expected closure for " ++ T.unpack name ++ ", got " ++ show other)
            Nothing -> expectationFailure ("missing " ++ T.unpack name)
      mapM_ expectClosure ["APPEND", "REVERSE", "MAP", "FILTER", "FOLDL"]

  -- ---------------------------------------------------------------
  -- SELF-EVALUATING LITERALS
  -- ---------------------------------------------------------------
  describe "literals" $ do
    it "integer" $
      "42" `shouldEvalTo` MI.MInt 42

    it "negative integer via sub" $
      "(sub 0 7)" `shouldEvalTo` MI.MInt (-7)

    it "float" $
      "3.14" `shouldEvalTo` MI.MFlt 3.14

    it "string" $
      "\"hello\"" `shouldEvalTo` MI.MStr "hello"

    it "empty string" $
      "\"\"" `shouldEvalTo` MI.MStr ""

    it "true" $
      "true" `shouldEvalTo` MI.MBool True

    it "false" $
      "false" `shouldEvalTo` MI.MBool False

  -- ---------------------------------------------------------------
  -- VARIABLE LOOKUP
  -- ---------------------------------------------------------------
  describe "variables" $ do
    it "looks up bound variable" $
      evalWith [("X", MI.MInt 99)] "x" `shouldBe` Right (MI.MInt 99)

    it "errors on undefined variable" $
      shouldFail "undefined-var"

  -- ---------------------------------------------------------------
  -- QUOTE
  -- ---------------------------------------------------------------
  describe "quote" $ do
    it "quotes an atom" $
      "(quote foo)" `shouldEvalTo` MI.MAtom "FOO"

    it "quotes a list" $
      "(quote (a b c))" `shouldEvalTo` MI.MList [MI.MAtom "A", MI.MAtom "B", MI.MAtom "C"]

    it "quotes nested structure" $
      "(quote (a (b c)))" `shouldEvalTo`
        MI.MList [MI.MAtom "A", MI.MList [MI.MAtom "B", MI.MAtom "C"]]

    it "does not evaluate contents" $
      evalWith [("X", MI.MInt 1)] "(quote x)" `shouldBe` Right (MI.MAtom "X")

  -- ---------------------------------------------------------------
  -- LET
  -- ---------------------------------------------------------------
  describe "let" $ do
    it "simple binding" $
      "(let ((x 42)) x)" `shouldEvalTo` MI.MInt 42

    it "multiple bindings, later sees earlier" $
      "(let ((x 1) (y x)) y)" `shouldEvalTo` MI.MInt 1

    it "body sees all bindings" $
      "(let ((a 10) (b 20)) (add a b))" `shouldEvalTo` MI.MInt 30

    it "nested let shadows" $
      "(let ((x 1)) (let ((x 2)) x))" `shouldEvalTo` MI.MInt 2

    it "recursive lambda binding" $
      "(let ((count-down (lam (n) (if (eq n 0) 0 (count-down (sub n 1)))))) (count-down 5))"
        `shouldEvalTo` MI.MInt 0

  -- ---------------------------------------------------------------
  -- LAMBDA
  -- ---------------------------------------------------------------
  describe "lambda" $ do
    it "creates and applies closure" $
      "(let ((f (lam (x) (add x 1)))) (f 10))" `shouldEvalTo` MI.MInt 11

    it "closure captures environment" $
      "(let ((a 100)) (let ((f (lam (x) (add x a)))) (f 5)))" `shouldEvalTo` MI.MInt 105

    it "multi-param lambda" $
      "(let ((f (lam (a b c) (add a (add b c))))) (f 1 2 3))" `shouldEvalTo` MI.MInt 6

    it "zero-param lambda" $
      "(let ((f (lam () 42))) (f))" `shouldEvalTo` MI.MInt 42

    it "wrong number of args errors" $
      shouldFail "(let ((f (lam (x) x))) (f 1 2))"

    it "application of non-function errors" $
      shouldFail "(42 1 2)"

    it "higher-order: function as argument" $
      "(let ((apply-fn (lam (f x) (f x)))) (apply-fn (lam (n) (add n 1)) 10))"
        `shouldEvalTo` MI.MInt 11

  -- ---------------------------------------------------------------
  -- IF
  -- ---------------------------------------------------------------
  describe "if" $ do
    it "true branch" $
      "(if true 1 2)" `shouldEvalTo` MI.MInt 1

    it "false branch" $
      "(if false 1 2)" `shouldEvalTo` MI.MInt 2

    it "non-boolean truthy: non-empty list is true" $
      "(if (list 1) 1 2)" `shouldEvalTo` MI.MInt 1

    it "non-boolean falsy: empty list is false" $
      "(if (list) 1 2)" `shouldEvalTo` MI.MInt 2

    it "non-boolean falsy: zero is false" $
      "(if 0 1 2)" `shouldEvalTo` MI.MInt 2

    it "non-boolean truthy: non-zero int is true" $
      "(if 42 1 2)" `shouldEvalTo` MI.MInt 1

    it "non-boolean falsy: empty string is false" $
      "(if \"\" 1 2)" `shouldEvalTo` MI.MInt 2

    it "non-boolean truthy: non-empty string is true" $
      "(if \"hi\" 1 2)" `shouldEvalTo` MI.MInt 1

  -- ---------------------------------------------------------------
  -- AND / OR
  -- ---------------------------------------------------------------
  describe "and" $ do
    it "all true" $
      "(and true true true)" `shouldEvalTo` MI.MBool True

    it "short-circuits on false" $
      "(and true false (error \"should not reach\"))" `shouldEvalTo` MI.MBool False

    it "empty and is true" $
      "(and)" `shouldEvalTo` MI.MBool True

    it "single value" $
      "(and 42)" `shouldEvalTo` MI.MInt 42

  describe "or" $ do
    it "first truthy wins" $
      "(or false 42 (error \"should not reach\"))" `shouldEvalTo` MI.MInt 42

    it "all false" $
      "(or false false false)" `shouldEvalTo` MI.MBool False

    it "empty or is false" $
      "(or)" `shouldEvalTo` MI.MBool False

    it "single value" $
      "(or 99)" `shouldEvalTo` MI.MInt 99

  -- ---------------------------------------------------------------
  -- ERROR CASES: INVALID FORMS
  -- ---------------------------------------------------------------
  describe "invalid forms" $ do
    it "invalid let: missing body" $
      shouldFail "(let ((x 1)))"

    it "invalid let: bindings not a list" $
      shouldFail "(let x 1)"

    it "invalid let: bad binding shape" $
      shouldFail "(let ((x)) x)"

    it "invalid lambda: no params list" $
      shouldFail "(lam x x)"

    it "invalid if: too few args" $
      shouldFail "(if true 1)"

    it "invalid if: too many args" $
      shouldFail "(if true 1 2 3)"

    it "quote: too many args" $
      shouldFail "(quote a b)"

    it "quote: no args" $
      shouldFail "(quote)"

    it "unquote outside quasiquote" $
      case evalWith [("X", MI.MInt 1)] ",x" of
        Left msg -> msg `shouldContain` "unquote"
        Right _ -> expectationFailure "expected error"

  -- ---------------------------------------------------------------
  -- QUASIQUOTE
  -- ---------------------------------------------------------------
  describe "quasiquote" $ do
    it "simple atom" $
      "`foo" `shouldEvalTo` MI.MAtom "FOO"

    it "simple list" $
      "`(a b c)" `shouldEvalTo` MI.MList [MI.MAtom "A", MI.MAtom "B", MI.MAtom "C"]

    it "with unquote" $
      evalWith [("X", MI.MInt 42)] "`,x"
        `shouldBe` Right (MI.MInt 42)

    it "unquote inside list" $
      evalWith [("X", MI.MInt 42)] "`(a ,x b)"
        `shouldBe` Right (MI.MList [MI.MAtom "A", MI.MInt 42, MI.MAtom "B"])

    it "splice into list" $
      evalWith [("XS", MI.MList [MI.MInt 1, MI.MInt 2, MI.MInt 3])] "`(a ,@xs b)"
        `shouldBe` Right (MI.MList [MI.MAtom "A", MI.MInt 1, MI.MInt 2, MI.MInt 3, MI.MAtom "B"])

    it "splice empty list" $
      evalWith [("XS", MI.MList [])] "`(a ,@xs b)"
        `shouldBe` Right (MI.MList [MI.MAtom "A", MI.MAtom "B"])

    it "unquote evaluates expression" $
      evalWith [("X", MI.MInt 3)] "`(result ,(add x 1))"
        `shouldBe` Right (MI.MList [MI.MAtom "RESULT", MI.MInt 4])

    it "nested list in quasiquote" $
      "`((a b) (c d))" `shouldEvalTo`
        MI.MList [MI.MList [MI.MAtom "A", MI.MAtom "B"],
                  MI.MList [MI.MAtom "C", MI.MAtom "D"]]

    it "splice of non-list errors" $
      case evalWith [("X", MI.MInt 1)] "`(a ,@x)" of
        Left _ -> pure ()
        Right v -> expectationFailure $ "expected error, got: " ++ show v

    it "preserves integers in quasiquote" $
      "`(a 42 b)" `shouldEvalTo` MI.MList [MI.MAtom "A", MI.MInt 42, MI.MAtom "B"]

    it "preserves strings in quasiquote" $
      "`(a \"hi\" b)" `shouldEvalTo` MI.MList [MI.MAtom "A", MI.MStr "hi", MI.MAtom "B"]

  -- ---------------------------------------------------------------
  -- BUILTINS: LIST OPERATIONS
  -- ---------------------------------------------------------------
  describe "car" $ do
    it "first element of list" $
      "(car (list 1 2 3))" `shouldEvalTo` MI.MInt 1

    it "of singleton" $
      "(car (list 42))" `shouldEvalTo` MI.MInt 42

    it "of empty list errors" $
      shouldFail "(car (list))"

    it "of non-list errors" $
      shouldFail "(car 42)"

  describe "cdr" $ do
    it "rest of list" $
      "(cdr (list 1 2 3))" `shouldEvalTo` MI.MList [MI.MInt 2, MI.MInt 3]

    it "of singleton gives empty" $
      "(cdr (list 1))" `shouldEvalTo` MI.MList []

    it "of empty list errors" $
      shouldFail "(cdr (list))"

    it "of non-list errors" $
      shouldFail "(cdr 42)"

  describe "cons" $ do
    it "prepend to list" $
      "(cons 1 (list 2 3))" `shouldEvalTo` MI.MList [MI.MInt 1, MI.MInt 2, MI.MInt 3]

    it "prepend to empty" $
      "(cons 1 (list))" `shouldEvalTo` MI.MList [MI.MInt 1]

    it "onto non-list errors" $
      shouldFail "(cons 1 42)"

  describe "list" $ do
    it "multiple args" $
      "(list 1 2 3)" `shouldEvalTo` MI.MList [MI.MInt 1, MI.MInt 2, MI.MInt 3]

    it "empty" $
      "(list)" `shouldEvalTo` MI.MList []

    it "single arg" $
      "(list 42)" `shouldEvalTo` MI.MList [MI.MInt 42]

    it "nested lists" $
      "(list (list 1) (list 2))" `shouldEvalTo`
        MI.MList [MI.MList [MI.MInt 1], MI.MList [MI.MInt 2]]

  describe "append" $ do
    it "two lists" $
      "(append (list 1 2) (list 3 4))" `shouldEvalTo`
        MI.MList [MI.MInt 1, MI.MInt 2, MI.MInt 3, MI.MInt 4]

    it "empty + non-empty" $
      "(append (list) (list 1 2))" `shouldEvalTo` MI.MList [MI.MInt 1, MI.MInt 2]

    it "non-empty + empty" $
      "(append (list 1 2) (list))" `shouldEvalTo` MI.MList [MI.MInt 1, MI.MInt 2]

    it "both empty" $
      "(append (list) (list))" `shouldEvalTo` MI.MList []

    it "non-list errors" $
      shouldFail "(append 1 (list))"

  describe "reverse" $ do
    it "reverses list" $
      "(reverse (list 1 2 3))" `shouldEvalTo` MI.MList [MI.MInt 3, MI.MInt 2, MI.MInt 1]

    it "empty" $
      "(reverse (list))" `shouldEvalTo` MI.MList []

  describe "length" $ do
    it "of list" $
      "(length (list 1 2 3))" `shouldEvalTo` MI.MInt 3

    it "of empty" $
      "(length (list))" `shouldEvalTo` MI.MInt 0

  -- ---------------------------------------------------------------
  -- BUILTINS: PREDICATES
  -- ---------------------------------------------------------------
  describe "null?" $ do
    it "empty list is true" $
      "(null? (list))" `shouldEvalTo` MI.MBool True

    it "non-empty is false" $
      "(null? (list 1))" `shouldEvalTo` MI.MBool False

  describe "symbol?" $ do
    it "atom is true" $
      "(symbol? (quote foo))" `shouldEvalTo` MI.MBool True

    it "int is false" $
      "(symbol? 42)" `shouldEvalTo` MI.MBool False

    it "string is false" $
      "(symbol? \"hi\")" `shouldEvalTo` MI.MBool False

  describe "list?" $ do
    it "list is true" $
      "(list? (list 1))" `shouldEvalTo` MI.MBool True

    it "empty list is true" $
      "(list? (list))" `shouldEvalTo` MI.MBool True

    it "atom is false" $
      "(list? (quote foo))" `shouldEvalTo` MI.MBool False

  describe "string?" $ do
    it "string is true" $
      "(string? \"hi\")" `shouldEvalTo` MI.MBool True

    it "atom is false" $
      "(string? (quote foo))" `shouldEvalTo` MI.MBool False

  describe "number?" $ do
    it "int is true" $
      "(number? 42)" `shouldEvalTo` MI.MBool True

    it "float is true" $
      "(number? 3.14)" `shouldEvalTo` MI.MBool True

    it "string is false" $
      "(number? \"hi\")" `shouldEvalTo` MI.MBool False

  describe "bool?" $ do
    it "true is true" $
      "(bool? true)" `shouldEvalTo` MI.MBool True

    it "false is true" $
      "(bool? false)" `shouldEvalTo` MI.MBool True

    it "int is false" $
      "(bool? 42)" `shouldEvalTo` MI.MBool False

  -- ---------------------------------------------------------------
  -- BUILTINS: EQUALITY AND LOGIC
  -- ---------------------------------------------------------------
  describe "eq" $ do
    it "equal atoms" $
      "(eq (quote foo) (quote foo))" `shouldEvalTo` MI.MBool True

    it "different atoms" $
      "(eq (quote foo) (quote bar))" `shouldEvalTo` MI.MBool False

    it "equal ints" $
      "(eq 42 42)" `shouldEvalTo` MI.MBool True

    it "different ints" $
      "(eq 1 2)" `shouldEvalTo` MI.MBool False

    it "equal strings" $
      "(eq \"hi\" \"hi\")" `shouldEvalTo` MI.MBool True

    it "different types" $
      "(eq 42 (quote foo))" `shouldEvalTo` MI.MBool False

    it "equal bools" $
      "(eq true true)" `shouldEvalTo` MI.MBool True

    it "equal empty lists" $
      "(eq (list) (list))" `shouldEvalTo` MI.MBool True

    it "equal non-empty lists" $
      "(eq (list 1 2) (list 1 2))" `shouldEvalTo` MI.MBool True

    it "different lists" $
      "(eq (list 1) (list 2))" `shouldEvalTo` MI.MBool False

  describe "not" $ do
    it "true -> false" $
      "(not true)" `shouldEvalTo` MI.MBool False

    it "false -> true" $
      "(not false)" `shouldEvalTo` MI.MBool True

  -- ---------------------------------------------------------------
  -- BUILTINS: HIGHER-ORDER FUNCTIONS
  -- ---------------------------------------------------------------
  describe "map" $ do
    it "over list" $
      "(map (lam (x) (add x 1)) (list 1 2 3))" `shouldEvalTo`
        MI.MList [MI.MInt 2, MI.MInt 3, MI.MInt 4]

    it "over empty" $
      "(map (lam (x) x) (list))" `shouldEvalTo` MI.MList []

    it "with builtin" $
      "(map car (list (list 1 2) (list 3 4)))" `shouldEvalTo`
        MI.MList [MI.MInt 1, MI.MInt 3]

  describe "filter" $ do
    it "filters list" $
      "(filter (lam (x) (not (eq x 2))) (list 1 2 3))" `shouldEvalTo`
        MI.MList [MI.MInt 1, MI.MInt 3]

    it "all pass" $
      "(filter (lam (x) true) (list 1 2 3))" `shouldEvalTo`
        MI.MList [MI.MInt 1, MI.MInt 2, MI.MInt 3]

    it "none pass" $
      "(filter (lam (x) false) (list 1 2 3))" `shouldEvalTo` MI.MList []

    it "empty list" $
      "(filter (lam (x) true) (list))" `shouldEvalTo` MI.MList []

  describe "foldl" $ do
    it "sum" $
      "(foldl (lam (acc x) (add acc x)) 0 (list 1 2 3))" `shouldEvalTo` MI.MInt 6

    it "empty list returns init" $
      "(foldl (lam (acc x) (add acc x)) 99 (list))" `shouldEvalTo` MI.MInt 99

    it "builds list in reverse" $
      "(foldl (lam (acc x) (cons x acc)) (list) (list 1 2 3))" `shouldEvalTo`
        MI.MList [MI.MInt 3, MI.MInt 2, MI.MInt 1]

  -- ---------------------------------------------------------------
  -- BUILTINS: STRING / SYMBOL CONVERSION
  -- ---------------------------------------------------------------
  describe "concat" $ do
    it "two strings" $
      "(concat \"hello\" \" world\")" `shouldEvalTo` MI.MStr "hello world"

    it "empty strings" $
      "(concat \"\" \"\")" `shouldEvalTo` MI.MStr ""

    it "non-string errors" $
      shouldFail "(concat 1 2)"

  describe "sym-to-str" $ do
    it "converts atom to string" $
      "(sym-to-str (quote foo))" `shouldEvalTo` MI.MStr "FOO"

    it "non-symbol errors" $
      shouldFail "(sym-to-str 42)"

  describe "str-to-sym" $ do
    it "converts string to atom" $
      "(str-to-sym \"FOO\")" `shouldEvalTo` MI.MAtom "FOO"

    it "non-string errors" $
      shouldFail "(str-to-sym 42)"

  -- ---------------------------------------------------------------
  -- BUILTINS: ARITHMETIC
  -- ---------------------------------------------------------------
  describe "add" $ do
    it "adds integers" $
      "(add 3 4)" `shouldEvalTo` MI.MInt 7

    it "negative result" $
      "(add 3 (sub 0 5))" `shouldEvalTo` MI.MInt (-2)

    it "non-integer errors" $
      shouldFail "(add \"a\" 1)"

  describe "sub" $ do
    it "subtracts integers" $
      "(sub 10 3)" `shouldEvalTo` MI.MInt 7

    it "negative result" $
      "(sub 3 10)" `shouldEvalTo` MI.MInt (-7)

  describe "mul" $ do
    it "multiplies integers" $
      "(mul 3 4)" `shouldEvalTo` MI.MInt 12

  describe "lt" $ do
    it "less than true" $
      "(lt 1 2)" `shouldEvalTo` MI.MBool True

    it "less than false" $
      "(lt 2 1)" `shouldEvalTo` MI.MBool False

    it "equal is false" $
      "(lt 1 1)" `shouldEvalTo` MI.MBool False

  describe "gt" $ do
    it "greater than true" $
      "(gt 2 1)" `shouldEvalTo` MI.MBool True

    it "greater than false" $
      "(gt 1 2)" `shouldEvalTo` MI.MBool False

  -- ---------------------------------------------------------------
  -- BUILTINS: GENSYM
  -- ---------------------------------------------------------------
  describe "gensym" $ do
    it "returns a symbol" $
      case evalSrc "(gensym)" of
        Right (MI.MAtom _) -> pure ()
        Right v -> expectationFailure $ "expected atom, got: " ++ show v
        Left e -> expectationFailure $ "eval failed: " ++ e

    it "successive gensyms are unique" $
      case evalSrc "(let ((a (gensym)) (b (gensym))) (eq a b))" of
        Right (MI.MBool False) -> pure ()
        Right v -> expectationFailure $ "expected false, got: " ++ show v
        Left e -> expectationFailure $ "eval failed: " ++ e

  -- ---------------------------------------------------------------
  -- BUILTINS: ERROR
  -- ---------------------------------------------------------------
  describe "error" $ do
    it "raises error with message" $
      case evalSrc "(error \"boom\")" of
        Left msg -> msg `shouldContain` "boom"
        Right _  -> expectationFailure "expected error"

  -- ---------------------------------------------------------------
  -- CONVERSION: MVal <-> SExpr
  -- ---------------------------------------------------------------
  describe "valToSExpr" $ do
    it "atom round-trips" $
      MI.valToSExpr (MI.MAtom "FOO") `shouldBe`
        Right (Loc.Located dummySp (SExpr.SAtom "FOO"))

    it "int round-trips" $
      MI.valToSExpr (MI.MInt 42) `shouldBe`
        Right (Loc.Located dummySp (SExpr.SInt 42))

    it "string round-trips" $
      MI.valToSExpr (MI.MStr "hi") `shouldBe`
        Right (Loc.Located dummySp (SExpr.SStr "hi"))

    it "bool true → SAtom TRUE" $
      MI.valToSExpr (MI.MBool True) `shouldBe`
        Right (Loc.Located dummySp (SExpr.SAtom "TRUE"))

    it "bool false → SAtom FALSE" $
      MI.valToSExpr (MI.MBool False) `shouldBe`
        Right (Loc.Located dummySp (SExpr.SAtom "FALSE"))

    it "list round-trips" $
      MI.valToSExpr (MI.MList [MI.MAtom "A", MI.MInt 1]) `shouldBe`
        Right (Loc.Located dummySp (SExpr.SList
          [Loc.Located dummySp (SExpr.SAtom "A"),
           Loc.Located dummySp (SExpr.SInt 1)]))

    it "closure errors" $
      case MI.valToSExpr (MI.MClosure M.empty [] (parseSExpr "1")) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error"

    it "builtin errors" $
      case MI.valToSExpr (MI.MBuiltin "X" (\_ -> MI.runInterpM (pure (MI.MInt 0)) `seq` undefined)) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error"

    it "float round-trips" $
      MI.valToSExpr (MI.MFlt 3.14) `shouldBe`
        Right (Loc.Located dummySp (SExpr.SFlt 3.14))

    it "type annotation round-trips" $ do
      let v = MI.MType (MI.MAtom "INT")
      MI.valToSExpr v `shouldBe`
        Right (Loc.Located dummySp (SExpr.SType (Loc.Located dummySp (SExpr.SAtom "INT"))))

  describe "sexprToVal" $ do
    it "atom" $
      MI.sexprToVal (parseSExpr "foo") `shouldBe` MI.MAtom "FOO"

    it "int" $
      MI.sexprToVal (parseSExpr "42") `shouldBe` MI.MInt 42

    it "string" $
      MI.sexprToVal (parseSExpr "\"hi\"") `shouldBe` MI.MStr "hi"

    it "true" $
      MI.sexprToVal (parseSExpr "true") `shouldBe` MI.MBool True

    it "list" $
      MI.sexprToVal (parseSExpr "(a 1)") `shouldBe`
        MI.MList [MI.MAtom "A", MI.MInt 1]

    it "false" $
      MI.sexprToVal (parseSExpr "false") `shouldBe` MI.MBool False

    it "float" $
      MI.sexprToVal (parseSExpr "3.14") `shouldBe` MI.MFlt 3.14

  -- ---------------------------------------------------------------
  -- UNINTERNED SYMBOLS
  -- ---------------------------------------------------------------
  describe "uninterned symbols" $ do
    it "usym is self-evaluating" $
      ":foo" `shouldEvalTo` MI.MUSym "FOO"

    it "usym in quasiquote" $
      "`:foo" `shouldEvalTo` MI.MUSym "FOO"

    it "usym in quasiquote list" $
      "`(:foo :bar)" `shouldEvalTo` MI.MList [MI.MUSym "FOO", MI.MUSym "BAR"]

    it "usym equality" $
      "(eq :foo :foo)" `shouldEvalTo` MI.MBool True

    it "usym inequality" $
      "(eq :foo :bar)" `shouldEvalTo` MI.MBool False

    it "usym not equal to string" $
      "(eq :foo \"FOO\")" `shouldEvalTo` MI.MBool False

    it "usym not equal to atom" $
      evalWith [("X", MI.MAtom "FOO")] "(eq :foo x)"
        `shouldBe` Right (MI.MBool False)

    it "usym in list operations" $
      "(car (list :foo :bar))" `shouldEvalTo` MI.MUSym "FOO"

    it "usym in cdr" $
      "(cdr (list :foo :bar))" `shouldEvalTo` MI.MList [MI.MUSym "BAR"]

    it "usym in splice" $
      "(let ((xs (list :foo :bar))) `(a ,@xs b))"
        `shouldEvalTo` MI.MList [MI.MAtom "A", MI.MUSym "FOO", MI.MUSym "BAR", MI.MAtom "B"]

    it "usym-to-str at macro time" $
      "(usym-to-str :foo)" `shouldEvalTo` MI.MStr "FOO"

    it "str-to-usym at macro time" $
      "(str-to-usym \"BAR\")" `shouldEvalTo` MI.MUSym "BAR"

  describe "usym sexprToVal" $ do
    it "converts SUSym to MUSym" $
      MI.sexprToVal (parseSExpr ":foo") `shouldBe` MI.MUSym "FOO"

  describe "usym valToSExpr" $ do
    it "converts MUSym to SUSym" $
      MI.valToSExpr (MI.MUSym "FOO") `shouldBe`
        Right (Loc.Located dummySp (SExpr.SUSym "FOO"))

  -- ---------------------------------------------------------------
  -- COMPOUND EXPRESSIONS
  -- ---------------------------------------------------------------
  describe "compound" $ do
    it "nested function calls" $
      "(car (cdr (list 1 2 3)))" `shouldEvalTo` MI.MInt 2

    it "let + lambda + application" $
      "(let ((double (lam (x) (add x x)))) (double (double 3)))" `shouldEvalTo` MI.MInt 12

    it "quasiquote builds code from computed parts" $ do
      let src = "(let ((name (quote MY-FN)) (val 42)) `(let ((,name ,val)) ,name))"
      src `shouldEvalTo`
        MI.MList [MI.MAtom "LET",
                  MI.MList [MI.MList [MI.MAtom "MY-FN", MI.MInt 42]],
                  MI.MAtom "MY-FN"]

    it "macro-like: build code with filter and map" $ do
      let src = T.unlines
            [ "(let ((decls (list (list (quote FLAG) (quote I) \"case insensitive\")"
            , "                   (list (quote ARG) (quote PATTERN) \"regex pattern\")"
            , "                   (list (quote FLAG) (quote V) \"invert\"))))"
            , "  (filter (lam (d) (eq (car d) (quote FLAG))) decls))"
            ]
      case evalSrc src of
        Right (MI.MList flags) -> length flags `shouldBe` 2
        Right v -> expectationFailure $ "expected list, got: " ++ show v
        Left e -> expectationFailure $ "eval failed: " ++ e

    it "recursive list processing" $ do
      let src = T.unlines
            [ "(let ((my-map (lam (f xs)"
            , "  (if (null? xs) (list)"
            , "    (cons (f (car xs)) (my-map f (cdr xs)))))))"
            , "  (my-map (lam (x) (add x 10)) (list 1 2 3)))"
            ]
      src `shouldEvalTo` MI.MList [MI.MInt 11, MI.MInt 12, MI.MInt 13]

  -- ---------------------------------------------------------------
  -- EDGE CASES
  -- ---------------------------------------------------------------
  describe "edge cases" $ do
    it "empty list is self-evaluating" $
      "(list)" `shouldEvalTo` MI.MList []

    it "deeply nested quasiquote" $
      evalWith [("X", MI.MInt 1)] "`((,x) ((,x ,x)))"
        `shouldBe` Right (MI.MList
          [MI.MList [MI.MInt 1],
           MI.MList [MI.MList [MI.MInt 1, MI.MInt 1]]])

    it "let binding shadows builtin" $
      "(let ((add (lam (x y) (sub x y)))) (add 10 3))" `shouldEvalTo` MI.MInt 7

    it "closure over mutable-like recursion" $ do
      let src = T.unlines
            [ "(let ((make-counter (lam (n)"
            , "  (list n (lam () (add n 1))))))"
            , "  (let ((c (make-counter 0)))"
            , "    (car c)))"
            ]
      src `shouldEvalTo` MI.MInt 0

    it "wrong arity for builtin errors" $
      shouldFail "(car 1 2)"

    it "map with non-list errors" $
      shouldFail "(map (lam (x) x) 42)"

    it "filter with non-list errors" $
      shouldFail "(filter (lam (x) x) 42)"

    it "foldl with non-list errors" $
      shouldFail "(foldl (lam (a x) a) 0 42)"

    it "length of non-list errors" $
      shouldFail "(length 42)"

    it "reverse of non-list errors" $
      shouldFail "(reverse 42)"

    it "append with non-list errors" $
      shouldFail "(append (list) 42)"

    it "macro returning closure errors in expansion" $
      case expandSrc "(mac bad () (lam (x) x)) (bad)" of
        Left msg -> msg `shouldContain` "closure"
        Right _ -> expectationFailure "expected error"

    it "type annotation preserved in sexprToVal" $ do
      -- %INT is SType (SAtom "INT") — preserved as MType
      let sx = Loc.Located dummySp (SExpr.SType (Loc.Located dummySp (SExpr.SAtom "INT")))
      MI.sexprToVal sx `shouldBe` MI.MType (MI.MAtom "INT")

  where
    dummySp = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)

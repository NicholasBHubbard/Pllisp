{-# LANGUAGE OverloadedStrings #-}

module CodegenSpec (spec) where

import Test.Hspec

import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.CST            as CST
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.TypeCheck      as TC

spec :: Spec
spec = do
  describe "integer arithmetic" $ do
    it "addition"       $ run "(print (int-to-str (add 1 2)))"   >>= (`shouldBe` "3")
    it "subtraction"    $ run "(print (int-to-str (sub 10 3)))"  >>= (`shouldBe` "7")
    it "multiplication" $ run "(print (int-to-str (mul 6 7)))"   >>= (`shouldBe` "42")
    it "division"       $ run "(print (int-to-str (div 10 3)))"  >>= (`shouldBe` "3")
    it "modulo"         $ run "(print (int-to-str (mod 10 3)))"  >>= (`shouldBe` "1")
    it "negation"       $ run "(print (int-to-str (neg 42)))"    >>= (`shouldBe` "-42")

  describe "float arithmetic" $ do
    it "addition"       $ run "(print (flt-to-str (addf 1.5 2.5)))" >>= (`shouldBe` "4")
    it "subtraction"    $ run "(print (flt-to-str (subf 5.0 1.5)))" >>= (`shouldBe` "3.5")
    it "multiplication" $ run "(print (flt-to-str (mulf 2.0 3.5)))" >>= (`shouldBe` "7")
    it "division"       $ run "(print (flt-to-str (divf 7.0 2.0)))" >>= (`shouldBe` "3.5")
    it "negation"       $ run "(print (flt-to-str (negf 3.14)))"    >>= (`shouldBe` "-3.14")

  describe "integer comparisons" $ do
    it "eq true"  $ run "(print (int-to-str (if (eq 1 1) 1 0)))" >>= (`shouldBe` "1")
    it "eq false" $ run "(print (int-to-str (if (eq 1 2) 1 0)))" >>= (`shouldBe` "0")
    it "lt"       $ run "(print (int-to-str (if (lt 1 2) 1 0)))" >>= (`shouldBe` "1")
    it "gt"       $ run "(print (int-to-str (if (gt 2 1) 1 0)))" >>= (`shouldBe` "1")
    it "le"       $ run "(print (int-to-str (if (le 1 1) 1 0)))" >>= (`shouldBe` "1")
    it "ge"       $ run "(print (int-to-str (if (ge 2 1) 1 0)))" >>= (`shouldBe` "1")

  describe "float comparisons" $ do
    it "eqf" $ run "(print (int-to-str (if (eqf 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "ltf" $ run "(print (int-to-str (if (ltf 1.0 2.0) 1 0)))" >>= (`shouldBe` "1")

  describe "boolean operations" $ do
    it "and tt" $ run "(print (int-to-str (if (and true true)   1 0)))" >>= (`shouldBe` "1")
    it "and tf" $ run "(print (int-to-str (if (and true false)  1 0)))" >>= (`shouldBe` "0")
    it "or tf"  $ run "(print (int-to-str (if (or true false)   1 0)))" >>= (`shouldBe` "1")
    it "or ff"  $ run "(print (int-to-str (if (or false false)  1 0)))" >>= (`shouldBe` "0")
    it "not"    $ run "(print (int-to-str (if (not false) 1 0)))"       >>= (`shouldBe` "1")

  describe "strings" $ do
    it "prints string literal"  $ run "(print \"hello\")"                      >>= (`shouldBe` "hello")
    it "concatenation"          $ run "(print (concat \"hello\" \" world\"))"  >>= (`shouldBe` "hello world")
    it "string length"          $ run "(print (int-to-str (strlen \"hello\")))" >>= (`shouldBe` "5")
    it "string equality true"   $ run "(print (int-to-str (if (eqs \"a\" \"a\") 1 0)))" >>= (`shouldBe` "1")
    it "string equality false"  $ run "(print (int-to-str (if (eqs \"a\" \"b\") 1 0)))" >>= (`shouldBe` "0")

  describe "conversions" $ do
    it "int to float"  $ run "(print (flt-to-str (int-to-flt 42)))" >>= (`shouldBe` "42")
    it "float to int"  $ run "(print (int-to-str (flt-to-int 3.7)))" >>= (`shouldBe` "3")
    it "int to string" $ run "(print (int-to-str 99))"              >>= (`shouldBe` "99")

  describe "let bindings" $ do
    it "simple let" $
      run "(let ((x 42)) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "multiple bindings" $
      run "(let ((x 1) (y 2)) (print (int-to-str (add x y))))" >>= (`shouldBe` "3")
    it "sequential binding" $
      run "(let ((x 10) (y (add x 5))) (print (int-to-str y)))" >>= (`shouldBe` "15")
    it "nested let" $
      run "(let ((x (let ((y 10)) (add y 1)))) (print (int-to-str x)))" >>= (`shouldBe` "11")

  describe "if expressions" $ do
    it "true branch"  $ run "(print (int-to-str (if true 1 2)))"  >>= (`shouldBe` "1")
    it "false branch" $ run "(print (int-to-str (if false 1 2)))" >>= (`shouldBe` "2")
    it "nested if" $
      run "(print (int-to-str (if true (if false 1 2) 3)))" >>= (`shouldBe` "2")

  describe "closures" $ do
    it "simple lambda" $
      run "(let ((f (lam ((x %INT)) (add x 1)))) (print (int-to-str (f 41))))"
        >>= (`shouldBe` "42")
    it "closure captures variable" $
      run "(let ((y 10)) (let ((f (lam ((x %INT)) (add x y)))) (print (int-to-str (f 32)))))"
        >>= (`shouldBe` "42")
    it "multi-param lambda" $
      run "(let ((f (lam ((x %INT) (y %INT)) (add x y)))) (print (int-to-str (f 20 22))))"
        >>= (`shouldBe` "42")
    it "higher-order function" $
      run (T.unlines
        [ "(let ((apply (lam (f x) (f x)))"
        , "      (double (lam ((x %INT)) (mul 2 x))))"
        , "  (print (int-to-str (apply double 21))))"
        ]) >>= (`shouldBe` "42")
    it "nested closures" $
      run (T.unlines
        [ "(let ((x 10))"
        , "  (let ((f (lam ((y %INT)) (lam ((z %INT)) (add x (add y z))))))"
        , "    (let ((g (f 20)))"
        , "      (print (int-to-str (g 12))))))"
        ]) >>= (`shouldBe` "42")

  describe "algebraic data types" $ do
    it "constructor and case match" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J 42)))"
        , "  (case x"
        , "    ((N) (print \"none\"))"
        , "    ((J v) (print (int-to-str v)))))"
        ]) >>= (`shouldBe` "42")
    it "zero-arg constructor" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x N))"
        , "  (case x"
        , "    ((N) (print \"nothing\"))"
        , "    ((J v) (print (int-to-str v)))))"
        ]) >>= (`shouldBe` "nothing")
    it "multiple constructors" $
      run (T.unlines
        [ "(type Color () (Red) (Green) (Blue))"
        , "(let ((c Green))"
        , "  (case c"
        , "    ((Red) (print \"r\"))"
        , "    ((Green) (print \"g\"))"
        , "    ((Blue) (print \"b\"))))"
        ]) >>= (`shouldBe` "g")

  describe "case expressions" $ do
    it "boolean case" $
      run "(case true (true (print \"yes\")) (false (print \"no\")))" >>= (`shouldBe` "yes")
    it "integer case" $
      run "(case 1 (1 (print \"one\")) (_ (print \"other\")))" >>= (`shouldBe` "one")
    it "wildcard case" $
      run "(case 42 (1 (print \"one\")) (_ (print \"other\")))" >>= (`shouldBe` "other")
    it "case with variable binding" $
      run "(case 42 (x (print (int-to-str x))))" >>= (`shouldBe` "42")

  describe "nested expressions" $ do
    it "nested arithmetic" $
      run "(print (int-to-str (add (mul 2 3) (sub 10 4))))" >>= (`shouldBe` "12")
    it "multiple top-level expressions" $
      run "(print \"first\") (print \"second\")" >>= (`shouldBe` "first\nsecond")

  describe "programs with no output" $ do
    it "no-op program" $ run "(add 1 2)" >>= (`shouldBe` "")
    it "unit literal" $ run "unit"        >>= (`shouldBe` "")

  -- EDGE CASES

  describe "arithmetic edge cases" $ do
    it "zero literal" $
      run "(print (int-to-str 0))" >>= (`shouldBe` "0")
    it "add zero" $
      run "(print (int-to-str (add 42 0)))" >>= (`shouldBe` "42")
    it "multiply by zero" $
      run "(print (int-to-str (mul 999 0)))" >>= (`shouldBe` "0")
    it "subtract self" $
      run "(print (int-to-str (sub 42 42)))" >>= (`shouldBe` "0")
    it "divide zero by nonzero" $
      run "(print (int-to-str (div 0 5)))" >>= (`shouldBe` "0")
    it "mod self" $
      run "(print (int-to-str (mod 7 7)))" >>= (`shouldBe` "0")
    it "negate zero" $
      run "(print (int-to-str (neg 0)))" >>= (`shouldBe` "0")
    it "double negation" $
      run "(print (int-to-str (neg (neg 42))))" >>= (`shouldBe` "42")
    it "large integer" $
      run "(print (int-to-str (mul 1000000 1000000)))" >>= (`shouldBe` "1000000000000")
    it "chained arithmetic" $
      run "(print (int-to-str (add (mul 2 (sub 10 3)) (div 100 (mod 7 4)))))"
        >>= (`shouldBe` "47")

  describe "float edge cases" $ do
    it "zero float" $
      run "(print (flt-to-str 0.0))" >>= (`shouldBe` "0")
    it "negate zero float" $
      run "(print (flt-to-str (negf 0.0)))" >>= (`shouldBe` "0")
    it "float subtract self" $
      run "(print (flt-to-str (subf 3.14 3.14)))" >>= (`shouldBe` "0")
    it "small float" $
      run "(print (flt-to-str 0.001))" >>= (`shouldBe` "0.001")
    it "large float" $
      run "(print (flt-to-str (mulf 1000000.0 1000000.0)))" >>= (`shouldBe` "1e+12")

  describe "comparison edge cases" $ do
    it "eq with zero" $
      run "(print (int-to-str (if (eq 0 0) 1 0)))" >>= (`shouldBe` "1")
    it "lt same value" $
      run "(print (int-to-str (if (lt 5 5) 1 0)))" >>= (`shouldBe` "0")
    it "le same value" $
      run "(print (int-to-str (if (le 5 5) 1 0)))" >>= (`shouldBe` "1")
    it "gt same value" $
      run "(print (int-to-str (if (gt 5 5) 1 0)))" >>= (`shouldBe` "0")
    it "ge same value" $
      run "(print (int-to-str (if (ge 5 5) 1 0)))" >>= (`shouldBe` "1")
    it "eqf different" $
      run "(print (int-to-str (if (eqf 1.0 1.1) 1 0)))" >>= (`shouldBe` "0")
    it "gtf" $
      run "(print (int-to-str (if (gtf 2.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "lef" $
      run "(print (int-to-str (if (lef 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "gef" $
      run "(print (int-to-str (if (gef 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")

  describe "boolean edge cases" $ do
    it "and ff" $
      run "(print (int-to-str (if (and false false) 1 0)))" >>= (`shouldBe` "0")
    it "or tt" $
      run "(print (int-to-str (if (or true true) 1 0)))" >>= (`shouldBe` "1")
    it "not true" $
      run "(print (int-to-str (if (not true) 1 0)))" >>= (`shouldBe` "0")
    it "double not" $
      run "(print (int-to-str (if (not (not true)) 1 0)))" >>= (`shouldBe` "1")
    it "chained and/or" $
      run "(print (int-to-str (if (or (and true false) (and true true)) 1 0)))"
        >>= (`shouldBe` "1")
    it "boolean in let binding" $
      run "(let ((b (eq 1 1))) (print (int-to-str (if b 42 0))))" >>= (`shouldBe` "42")

  describe "string edge cases" $ do
    it "empty string" $
      run "(print \"\")" >>= (`shouldBe` "")
    it "empty string length" $
      run "(print (int-to-str (strlen \"\")))" >>= (`shouldBe` "0")
    it "concat with empty left" $
      run "(print (concat \"\" \"hello\"))" >>= (`shouldBe` "hello")
    it "concat with empty right" $
      run "(print (concat \"hello\" \"\"))" >>= (`shouldBe` "hello")
    it "concat both empty" $
      run "(print (concat \"\" \"\"))" >>= (`shouldBe` "")
    it "eqs empty strings" $
      run "(print (int-to-str (if (eqs \"\" \"\") 1 0)))" >>= (`shouldBe` "1")
    it "eqs empty vs nonempty" $
      run "(print (int-to-str (if (eqs \"\" \"a\") 1 0)))" >>= (`shouldBe` "0")
    it "strlen after concat" $
      run "(print (int-to-str (strlen (concat \"ab\" \"cde\"))))" >>= (`shouldBe` "5")
    it "multiple concats" $
      run "(print (concat (concat \"a\" \"b\") (concat \"c\" \"d\")))" >>= (`shouldBe` "abcd")
    it "string deduplication" $
      run "(print (concat \"x\" \"x\"))" >>= (`shouldBe` "xx")

  describe "conversion edge cases" $ do
    it "int-to-flt zero" $
      run "(print (flt-to-str (int-to-flt 0)))" >>= (`shouldBe` "0")
    it "flt-to-int zero" $
      run "(print (int-to-str (flt-to-int 0.0)))" >>= (`shouldBe` "0")
    it "flt-to-int truncates" $
      run "(print (int-to-str (flt-to-int 9.9)))" >>= (`shouldBe` "9")
    it "flt-to-int negative" $
      run "(print (int-to-str (flt-to-int (negf 3.7))))" >>= (`shouldBe` "-3")
    it "int-to-str negative" $
      run "(print (int-to-str (neg 1)))" >>= (`shouldBe` "-1")
    it "int-to-str zero" $
      run "(print (int-to-str 0))" >>= (`shouldBe` "0")
    it "roundtrip int-flt-int" $
      run "(print (int-to-str (flt-to-int (int-to-flt 42))))" >>= (`shouldBe` "42")

  describe "let binding edge cases" $ do
    it "variable shadowing" $
      run "(let ((x 1)) (let ((x 2)) (print (int-to-str x))))" >>= (`shouldBe` "2")
    it "outer var restored after shadow" $
      run (T.unlines
        [ "(let ((x 1))"
        , "  (let ((y (let ((x 99)) x)))"
        , "    (print (int-to-str (add x y)))))"
        ]) >>= (`shouldBe` "100")
    it "binding used multiple times" $
      run "(let ((x 5)) (print (int-to-str (add x (add x x)))))" >>= (`shouldBe` "15")
    it "deeply nested let" $
      run (T.unlines
        [ "(let ((a 1))"
        , "  (let ((b (add a 1)))"
        , "    (let ((c (add b 1)))"
        , "      (let ((d (add c 1)))"
        , "        (print (int-to-str d))))))"
        ]) >>= (`shouldBe` "4")
    it "let binding from if" $
      run "(let ((x (if true 42 0))) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "let binding from case" $
      run "(let ((x (case 5 (5 42) (_ 0)))) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "let with float binding" $
      run "(let ((x 3.14)) (print (flt-to-str x)))" >>= (`shouldBe` "3.14")
    it "let with string binding" $
      run "(let ((s \"hello\")) (print s))" >>= (`shouldBe` "hello")
    it "let with bool binding" $
      run "(let ((b true)) (print (int-to-str (if b 1 0))))" >>= (`shouldBe` "1")

  describe "if edge cases" $ do
    it "deeply nested if" $
      run "(print (int-to-str (if true (if true (if true (if false 0 42) 0) 0) 0)))"
        >>= (`shouldBe` "42")
    it "if as function argument" $
      run "(print (int-to-str (add (if true 10 0) (if false 0 32))))"
        >>= (`shouldBe` "42")
    it "if returning string" $
      run "(print (if true \"yes\" \"no\"))" >>= (`shouldBe` "yes")
    it "if returning float" $
      run "(print (flt-to-str (if true 1.5 2.5)))" >>= (`shouldBe` "1.5")
    it "if with comparison condition" $
      run "(print (int-to-str (if (gt (add 1 2) 2) 42 0)))" >>= (`shouldBe` "42")
    it "if branches with side effects" $
      run "(if true (print \"a\") (print \"b\"))" >>= (`shouldBe` "a")

  describe "case edge cases" $ do
    it "case with only wildcard" $
      run "(case 42 (_ (print \"catch-all\")))" >>= (`shouldBe` "catch-all")
    it "case with only variable" $
      run "(case 99 (n (print (int-to-str n))))" >>= (`shouldBe` "99")
    it "case inside if" $
      run "(print (int-to-str (if true (case 1 (1 42) (_ 0)) 0)))"
        >>= (`shouldBe` "42")
    it "if inside case" $
      run "(case true (true (print (int-to-str (if (eq 1 1) 42 0)))) (false (print \"no\")))"
        >>= (`shouldBe` "42")
    it "case result in arithmetic" $
      run "(print (int-to-str (add (case 1 (1 10) (_ 0)) (case 2 (2 32) (_ 0)))))"
        >>= (`shouldBe` "42")
    it "boolean false case" $
      run "(case false (true (print \"yes\")) (false (print \"no\")))"
        >>= (`shouldBe` "no")
    it "integer case with multiple arms" $
      run "(case 3 (1 (print \"one\")) (2 (print \"two\")) (3 (print \"three\")) (_ (print \"other\")))"
        >>= (`shouldBe` "three")
    it "case falls through to wildcard" $
      run "(case 99 (1 (print \"one\")) (2 (print \"two\")) (_ (print \"other\")))"
        >>= (`shouldBe` "other")
    it "nested case" $
      run (T.unlines
        [ "(case 1"
        , "  (1 (case true"
        , "    (true (print \"1-true\"))"
        , "    (false (print \"1-false\"))))"
        , "  (_ (print \"other\")))"
        ]) >>= (`shouldBe` "1-true")

  describe "closure edge cases" $ do
    it "lambda returning bool" $
      run (T.unlines
        [ "(let ((is-zero (lam ((x %INT)) (eq x 0))))"
        , "  (print (int-to-str (if (is-zero 0) 1 0))))"
        ]) >>= (`shouldBe` "1")
    it "lambda returning string" $
      run (T.unlines
        [ "(let ((greeting (lam ((name %STR)) (concat \"hello \" name))))"
        , "  (print (greeting \"world\")))"
        ]) >>= (`shouldBe` "hello world")
    it "lambda returning float" $
      run (T.unlines
        [ "(let ((half (lam ((x %FLT)) (divf x 2.0))))"
        , "  (print (flt-to-str (half 10.0))))"
        ]) >>= (`shouldBe` "5")
    it "closure called multiple times" $
      run (T.unlines
        [ "(let ((inc (lam ((x %INT)) (add x 1))))"
        , "  (print (int-to-str (add (inc 1) (inc 2)))))"
        ]) >>= (`shouldBe` "5")
    it "two closures sharing captured var" $
      run (T.unlines
        [ "(let ((base 10))"
        , "  (let ((f (lam ((x %INT)) (add base x)))"
        , "        (g (lam ((x %INT)) (mul base x))))"
        , "    (print (int-to-str (add (f 5) (g 3))))))"
        ]) >>= (`shouldBe` "45")
    it "closure capturing closure" $
      run (T.unlines
        [ "(let ((f (lam ((x %INT)) (add x 1))))"
        , "  (let ((g (lam ((y %INT)) (f (f y)))))"
        , "    (print (int-to-str (g 40)))))"
        ]) >>= (`shouldBe` "42")
    it "closure returning unit" $
      run (T.unlines
        [ "(let ((say (lam ((s %STR)) (print s))))"
        , "  (say \"hello\"))"
        ]) >>= (`shouldBe` "hello")
    it "identity lambda" $
      run "(let ((id (lam (x) x))) (print (int-to-str (id 42))))"
        >>= (`shouldBe` "42")

  describe "ADT edge cases" $ do
    it "constructor with two fields" $
      run (T.unlines
        [ "(type Pair (a b) (MkPair a b))"
        , "(let ((p (MkPair 10 32)))"
        , "  (case p"
        , "    ((MkPair x y) (print (int-to-str (add x y))))))"
        ]) >>= (`shouldBe` "42")
    it "multiple types in same program" $
      run (T.unlines
        [ "(type Bool2 () (T) (F))"
        , "(type M (a) (N) (J a))"
        , "(let ((b T) (m (J 42)))"
        , "  (case b"
        , "    ((T) (case m"
        , "      ((N) (print \"none\"))"
        , "      ((J v) (print (int-to-str v)))))"
        , "    ((F) (print \"false\"))))"
        ]) >>= (`shouldBe` "42")
    it "ADT inside ADT" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J (J 42))))"
        , "  (case x"
        , "    ((N) (print \"outer-none\"))"
        , "    ((J inner) (case inner"
        , "      ((N) (print \"inner-none\"))"
        , "      ((J v) (print (int-to-str v)))))))"
        ]) >>= (`shouldBe` "42")
    it "mixed-arity constructor arms" $
      run (T.unlines
        [ "(type Shape () (Circle %FLT) (Rect %FLT %FLT) (Point))"
        , "(let ((s (Rect 3.0 4.0)))"
        , "  (case s"
        , "    ((Circle r) (print (flt-to-str r)))"
        , "    ((Rect w h) (print (flt-to-str (mulf w h))))"
        , "    ((Point) (print \"point\"))))"
        ]) >>= (`shouldBe` "12")
    it "zero-arg constructor used twice" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x N) (y N))"
        , "  (case x"
        , "    ((N) (case y"
        , "      ((N) (print \"both-none\"))"
        , "      ((J v) (print \"unreachable\"))))"
        , "    ((J v) (print \"unreachable\"))))"
        ]) >>= (`shouldBe` "both-none")
    it "constructor in let binding" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J 42)))"
        , "  (let ((val (case x"
        , "    ((N) 0)"
        , "    ((J v) v))))"
        , "    (print (int-to-str val))))"
        ]) >>= (`shouldBe` "42")

  describe "complex integration" $ do
    it "fibonacci-style chain" $
      run (T.unlines
        [ "(let ((a 1) (b 1))"
        , "  (let ((c (add a b)))"
        , "    (let ((d (add b c)))"
        , "      (let ((e (add c d)))"
        , "        (print (int-to-str e))))))"
        ]) >>= (`shouldBe` "5")
    it "map-like pattern" $
      run (T.unlines
        [ "(let ((f (lam ((x %INT)) (mul x x))))"
        , "  (print (int-to-str (add (f 3) (f 4)))))"
        ]) >>= (`shouldBe` "25")
    it "closure as data in ADT" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((f (lam ((x %INT)) (add x 1))))"
        , "  (let ((wrapped (J f)))"
        , "    (case wrapped"
        , "      ((N) (print \"none\"))"
        , "      ((J g) (print (int-to-str (g 41)))))))"
        ]) >>= (`shouldBe` "42")
    it "three top-level prints" $
      run "(print \"a\") (print \"b\") (print \"c\")" >>= (`shouldBe` "a\nb\nc")
    it "let and if and case combined" $
      run (T.unlines
        [ "(let ((x (if (eq 1 1) 5 0)))"
        , "  (case x"
        , "    (5 (print \"five\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "five")

-- HELPERS

pipeline :: T.Text -> T.Text
pipeline src = case Parser.parseProgram "<test>" src of
  Left e -> error ("parse error: " ++ show e)
  Right prog -> case Resolve.resolve (CST.progExprs prog) of
    Left e -> error ("resolve error: " ++ show e)
    Right resolved -> case TC.typecheck resolved of
      Left e -> error ("typecheck error: " ++ show e)
      Right typed ->
        Codegen.codegen (LL.lambdaLift (CC.closureConvert typed))

run :: T.Text -> IO String
run src = do
  let ir = pipeline src
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  (ec1, _, err1) <- readProcessWithExitCode
    "llc" ["/tmp/pllisp_test.ll", "-o", "/tmp/pllisp_test.o", "-filetype=obj"] ""
  case ec1 of
    ExitFailure _ -> error ("llc failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, _, err2) <- readProcessWithExitCode
        "gcc" ["/tmp/pllisp_test.o", "-o", "/tmp/pllisp_test_exe", "-lm"] ""
      case ec2 of
        ExitFailure _ -> error ("gcc link failed:\n" ++ err2)
        ExitSuccess -> do
          (ec3, out, err3) <- readProcessWithExitCode "/tmp/pllisp_test_exe" [] ""
          case ec3 of
            ExitSuccess   -> pure (strip out)
            ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err3)
  where
    strip = reverse . dropWhile (== '\n') . reverse

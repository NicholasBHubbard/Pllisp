{-# LANGUAGE OverloadedStrings #-}

module CodegenSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO as T.IO
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.CST            as CST
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.MacroExpand    as MacroExpand
import qualified Pllisp.Module         as Mod
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.Stdlib         as Stdlib
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

  describe "recursive closures" $ do
    it "recursive sum" $
      run (T.unlines
        [ "(let ((sum (lam ((n %INT))"
        , "  (if (eq n 0) 0 (add n (sum (sub n 1)))))))"
        , "  (print (int-to-str (sum 5))))"
        ]) >>= (`shouldBe` "15")
    it "recursive countdown" $
      run (T.unlines
        [ "(let ((countdown (lam ((n %INT))"
        , "  (if (eq n 0)"
        , "    (print \"done\")"
        , "    (let ((_ (print (int-to-str n))))"
        , "      (countdown (sub n 1)))))))"
        , "  (countdown 3))"
        ]) >>= (`shouldBe` "3\n2\n1\ndone")
    it "recursive with captured variable" $
      run (T.unlines
        [ "(let ((base 10))"
        , "  (let ((f (lam ((n %INT))"
        , "    (if (eq n 0) base (f (sub n 1))))))"
        , "    (print (int-to-str (f 5)))))"
        ]) >>= (`shouldBe` "10")
    it "recursive factorial" $
      run (T.unlines
        [ "(let ((fact (lam ((n %INT))"
        , "  (if (eq n 0) 1 (mul n (fact (sub n 1)))))))"
        , "  (print (int-to-str (fact 6))))"
        ]) >>= (`shouldBe` "720")
    it "recursive with dummy arg (loop pattern)" $
      run (T.unlines
        [ "(let ((n 3))"
        , "  (let ((loop (lam ((dummy %INT))"
        , "    (if (eq n 0) unit"
        , "      (let ((_ (print (int-to-str n))))"
        , "        unit)))))"
        , "    (loop 0)))"
        ]) >>= (`shouldBe` "3")

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

  describe "new built-ins" $ do
    it "str-contains found" $
      run "(print (int-to-str (if (str-contains \"hello world\" \"world\") 1 0)))"
        >>= (`shouldBe` "1")
    it "str-contains not found" $
      run "(print (int-to-str (if (str-contains \"hello world\" \"xyz\") 1 0)))"
        >>= (`shouldBe` "0")
    it "str-contains empty needle" $
      run "(print (int-to-str (if (str-contains \"hello\" \"\") 1 0)))"
        >>= (`shouldBe` "1")
    it "str-contains empty haystack" $
      run "(print (int-to-str (if (str-contains \"\" \"a\") 1 0)))"
        >>= (`shouldBe` "0")
    it "substr" $
      run "(print (substr \"hello world\" 6 5))" >>= (`shouldBe` "world")
    it "substr from start" $
      run "(print (substr \"hello\" 0 3))" >>= (`shouldBe` "hel")
    it "substr zero length" $
      run "(print (concat \"[\" (concat (substr \"hello\" 0 0) \"]\")))"
        >>= (`shouldBe` "[]")
    it "argc" $
      run "(print (int-to-str (argc unit)))" >>= (`shouldBe` "1")
    it "argv 0" $
      runWith ["foo", "bar"] "(print (argv 1))" >>= (`shouldBe` "foo")
    it "read-line and is-eof" $
      runWithInput "hello\n" (T.unlines
        [ "(let ((line (read-line unit)))"
        , "  (print line))"
        ]) >>= (`shouldBe` "hello")
    it "is-eof after exhausted input" $
      runWithInput "" (T.unlines
        [ "(let ((line (read-line unit)))"
        , "  (print (int-to-str (if (is-eof unit) 1 0))))"
        ]) >>= (`shouldBe` "1")
    it "read-line multiple lines" $
      runWithInput "first\nsecond\n" (T.unlines
        [ "(let ((a (read-line unit)))"
        , "  (let ((b (read-line unit)))"
        , "    (let ((_ (print a)))"
        , "      (print b))))"
        ]) >>= (`shouldBe` "first\nsecond")
    it "is-eof false before exhaustion" $
      runWithInput "hello\n" (T.unlines
        [ "(let ((_ (print (int-to-str (if (is-eof unit) 1 0)))))"
        , "  (let ((line (read-line unit)))"
        , "    (print line)))"
        ]) >>= (`shouldBe` "0\nhello")
    it "argc with extra args" $
      runWith ["x", "y"] "(print (int-to-str (argc unit)))" >>= (`shouldBe` "3")
    it "argv second arg" $
      runWith ["hello", "world"] "(print (argv 2))" >>= (`shouldBe` "world")
    it "substr full string" $
      run "(print (substr \"hello\" 0 5))" >>= (`shouldBe` "hello")
    it "str-contains self" $
      run "(print (int-to-str (if (str-contains \"hello\" \"hello\") 1 0)))"
        >>= (`shouldBe` "1")

  describe "built-in List type" $ do
    it "constructs Nil" $
      run "(case Nil ((Nil) (print \"empty\")))" >>= (`shouldBe` "empty")

    it "constructs Cons and pattern matches" $
      run (T.unlines
        [ "(let ((xs (Cons 1 (Cons 2 (Cons 3 Nil)))))"
        , "  (case xs"
        , "    ((Cons h _) (print (int-to-str h)))))"
        ]) >>= (`shouldBe` "1")

    it "recursive list traversal" $
      run (T.unlines
        [ "(let ((sum (lam ((xs %(List %INT))) (case xs"
        , "    ((Nil) 0)"
        , "    ((Cons h t) (add h (sum t)))))))"
        , "  (print (int-to-str (sum (Cons 10 (Cons 20 (Cons 30 Nil)))))))"
        ]) >>= (`shouldBe` "60")

  describe "rx" $ do
    it "rx-match positive" $
      run "(print (int-to-str (if (rx-match /^[0-9]+$/ \"12345\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx-match negative" $
      run "(print (int-to-str (if (rx-match /^[0-9]+$/ \"abc\") 1 0)))"
        >>= (`shouldBe` "0")

    it "rx-find extracts first match" $
      run "(print (rx-find /[0-9]+/ \"foo123bar456\"))"
        >>= (`shouldBe` "123")

    it "rx-find no match returns empty string" $
      run "(print (concat \"[\" (concat (rx-find /[0-9]+/ \"abc\") \"]\")))"
        >>= (`shouldBe` "[]")

    it "rx-sub replaces first match" $
      run "(print (rx-sub /[0-9]+/ \"X\" \"a1b2c3\"))"
        >>= (`shouldBe` "aXb2c3")

    it "rx-gsub replaces all matches" $
      run "(print (rx-gsub /[0-9]+/ \"X\" \"a1b2c3\"))"
        >>= (`shouldBe` "aXbXcX")

    it "rx-sub with backreference" $
      run "(print (rx-sub /(\\w+)@(\\w+)/ \"$2/$1\" \"user@host\"))"
        >>= (`shouldBe` "host/user")

    it "rx-split on comma" $ do
      run (T.unlines
        [ "(let ((parts (rx-split /,/ \"a,b,c\")))"
        , "  (case parts"
        , "    ((Cons a _) (print a))))"
        ]) >>= (`shouldBe` "a")

    it "rx-split prints all parts" $ do
      run (T.unlines
        [ "(let ((print-list (lam ((xs %(List %STR))) (case xs"
        , "    ((Nil) unit)"
        , "    ((Cons h t) (let ((_ (print h))) (print-list t)))))))"
        , "  (print-list (rx-split /,/ \"x,y,z\")))"
        ]) >>= (`shouldBe` "x\ny\nz")

    it "rx-captures extracts groups" $ do
      run (T.unlines
        [ "(let ((caps (rx-captures /(\\w+)@(\\w+)/ \"user@host\")))"
        , "  (case caps"
        , "    ((Cons user rest) (case rest"
        , "      ((Cons host _) (print (concat user (concat \"@\" host))))))))"
        ]) >>= (`shouldBe` "user@host")

    it "rx-captures no match returns Nil" $ do
      run (T.unlines
        [ "(case (rx-captures /(\\d+)/ \"abc\")"
        , "  ((Nil) (print \"none\"))"
        , "  ((Cons _ _) (print \"found\")))"
        ]) >>= (`shouldBe` "none")

    it "rx with case-insensitive flag" $
      run "(print (int-to-str (if (rx-match /hello/i \"HELLO\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx stored in let binding" $
      run "(let ((re /[0-9]+/)) (print (rx-find re \"abc123\")))"
        >>= (`shouldBe` "123")

    it "rx-compile from string" $
      run "(let ((re (rx-compile \"[0-9]+\"))) (print (rx-find re \"abc123\")))"
        >>= (`shouldBe` "123")

    it "rx-compile used in match" $
      run "(let ((re (rx-compile \"^hello$\"))) (print (int-to-str (if (rx-match re \"hello\") 1 0))))"
        >>= (`shouldBe` "1")

    it "rx with \\Q\\E literal quoting" $
      run "(print (int-to-str (if (rx-match /\\Qfoo.bar\\E/ \"foo.bar\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx \\Q\\E does not match metachar" $
      run "(print (int-to-str (if (rx-match /\\Qfoo.bar\\E/ \"fooXbar\") 1 0)))"
        >>= (`shouldBe` "0")

  describe "module imports" $ do
    it "unqualified import" $ do
      let modSrc = "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
          mainSrc = "(print (int-to-str (square 5)))"
      runWithModule "MOD" modSrc ["SQUARE"] mainSrc >>= (`shouldBe` "25")

    it "qualified import" $ do
      let modSrc = "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
          mainSrc = "(print (int-to-str (Mod.square 7)))"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "49")

    it "imported type constructors" $ do
      let modSrc = "(type Box (a) (MkBox a))"
          mainSrc = T.unlines
            [ "(let ((b (MkBox 42)))"
            , "  (case b ((MkBox x) (print (int-to-str x)))))"
            ]
      runWithModule "MOD" modSrc ["MKBOX"] mainSrc >>= (`shouldBe` "42")

    it "multiple bindings from one module" $ do
      let modSrc = T.unlines
            [ "(let ((double (lam ((x %INT)) (mul 2 x)))"
            , "      (square (lam ((x %INT)) (mul x x))))"
            , "  unit)"
            ]
          mainSrc = "(print (int-to-str (square (double 3))))"
      runWithModule "MOD" modSrc ["SQUARE", "DOUBLE"] mainSrc >>= (`shouldBe` "36")

  describe "macros" $ do
    it "when macro" $ do
      run (T.unlines
        [ "(mac when (test body) `(if ,test ,body unit))"
        , "(when true (print \"yes\"))"
        ]) >>= (`shouldBe` "yes")

    it "when macro false branch" $ do
      run (T.unlines
        [ "(mac when (test body) `(if ,test ,body unit))"
        , "(when false (print \"no\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

    it "unless macro" $ do
      run (T.unlines
        [ "(mac unless (test body) `(if ,test unit ,body))"
        , "(unless false (print \"ran\"))"
        ]) >>= (`shouldBe` "ran")

    it "do macro sequences expressions" $ do
      run (T.unlines
        [ "(mac do (expr) expr)"
        , "(mac do (first &rest rest) `(let ((_ ,first)) (do ,@rest)))"
        , "(do (print \"a\") (print \"b\") (print \"c\"))"
        ]) >>= (`shouldBe` "a\nb\nc")

    it "cond macro" $ do
      run (T.unlines
        [ "(mac cond ((test body)) `(if ,test ,body unit))"
        , "(mac cond ((test body) &rest rest) `(if ,test ,body (cond ,@rest)))"
        , "(let ((x 2))"
        , "  (cond ((eq x 1) (print \"one\"))"
        , "        ((eq x 2) (print \"two\"))"
        , "        ((eq x 3) (print \"three\"))))"
        ]) >>= (`shouldBe` "two")

    it "macro using another macro" $ do
      run (T.unlines
        [ "(mac unless (test body) `(if ,test unit ,body))"
        , "(mac when (test body) `(unless (not ,test) ,body))"
        , "(when true (print \"ok\"))"
        ]) >>= (`shouldBe` "ok")

    it "macro with type annotations" $ do
      run (T.unlines
        [ "(mac typed-id (x) `,x)"
        , "(print (int-to-str (typed-id (add 1 2))))"
        ]) >>= (`shouldBe` "3")

    it "macro in nested position" $ do
      run (T.unlines
        [ "(mac double (x) `(add ,x ,x))"
        , "(print (int-to-str (double 21)))"
        ]) >>= (`shouldBe` "42")

    it "macro with destructuring params" $ do
      run (T.unlines
        [ "(mac first-of ((a b)) `,a)"
        , "(print (int-to-str (first-of (42 99))))"
        ]) >>= (`shouldBe` "42")

  describe "record types" $ do
    it "record field access string" $
      run (T.unlines
        [ "(type Person () (Person (name %STR) (age %INT)))"
        , "(let ((p (Person \"alice\" 30)))"
        , "  (print (.name p)))"
        ]) >>= (`shouldBe` "alice")

    it "record field access int" $
      run (T.unlines
        [ "(type Person () (Person (name %STR) (age %INT)))"
        , "(let ((p (Person \"alice\" 30)))"
        , "  (print (int-to-str (.age p))))"
        ]) >>= (`shouldBe` "30")

    it "record pattern match still works" $
      run (T.unlines
        [ "(type Point () (Point (x %INT) (y %INT)))"
        , "(let ((p (Point 3 4)))"
        , "  (case p ((Point a b) (print (int-to-str (add a b))))))"
        ]) >>= (`shouldBe` "7")

    it "record with type parameter" $
      run (T.unlines
        [ "(type Box (a) (Box (val a)))"
        , "(let ((b (Box 42)))"
        , "  (print (int-to-str (.val b))))"
        ]) >>= (`shouldBe` "42")

    it "nested field access" $
      run (T.unlines
        [ "(type Box (a) (Box (val a)))"
        , "(let ((b (Box (Box 42))))"
        , "  (print (int-to-str (.val (.val b)))))"
        ]) >>= (`shouldBe` "42")

  describe "&rest" $ do
    it "captures remaining args as list" $
      run (T.unlines
        [ "(let ((f (lam (a &rest xs) (case xs"
        , "  ((Cons h _) (print (int-to-str (add a h))))"
        , "  ((Nil) (print (int-to-str a)))))))"
        , "  (f 10 20))"
        ]) >>= (`shouldBe` "30")

    it "no extra args gives Nil" $
      run (T.unlines
        [ "(let ((f (lam (a &rest xs) (case xs"
        , "  ((Nil) (print \"empty\"))"
        , "  ((Cons _ _) (print \"notempty\"))))))"
        , "  (f 1))"
        ]) >>= (`shouldBe` "empty")

    it "captures multiple extra args" $
      run (T.unlines
        [ "(let ((go (lam ((xs %(List %INT))) (case xs"
        , "  ((Nil) 0)"
        , "  ((Cons h t) (add h (go t)))))))"
        , "  (let ((sum-all (lam (&rest xs) (go xs))))"
        , "    (print (int-to-str (sum-all 1 2 3 4)))))"
        ]) >>= (`shouldBe` "10")

    it "rest with no required params" $
      run (T.unlines
        [ "(let ((f (lam (&rest xs) (case xs"
        , "  ((Nil) (print \"none\"))"
        , "  ((Cons h _) (print (int-to-str h)))))))"
        , "  (f 42))"
        ]) >>= (`shouldBe` "42")

  describe "%opt" $ do
    it "uses default when arg omitted" $
      run (T.unlines
        [ "(let ((f (lam (a %opt (b 10)) (add a b))))"
        , "  (print (int-to-str (f 5))))"
        ]) >>= (`shouldBe` "15")

    it "uses provided arg over default" $
      run (T.unlines
        [ "(let ((f (lam (a %opt (b 10)) (add a b))))"
        , "  (print (int-to-str (f 5 20))))"
        ]) >>= (`shouldBe` "25")

    it "multiple optional params all defaulted" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f))))"
        ]) >>= (`shouldBe` "6")

    it "multiple optional params partially provided" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f 10))))"
        ]) >>= (`shouldBe` "15")

    it "multiple optional params all provided" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f 10 20 30))))"
        ]) >>= (`shouldBe` "60")

    it "opt with string default" $
      run (T.unlines
        [ "(let ((greet (lam (%opt (name \"world\")) (concat \"hello \" name))))"
        , "  (print (greet)))"
        ]) >>= (`shouldBe` "hello world")

    it "opt with string provided" $
      run (T.unlines
        [ "(let ((greet (lam (%opt (name \"world\")) (concat \"hello \" name))))"
        , "  (print (greet \"alice\")))"
        ]) >>= (`shouldBe` "hello alice")

  describe "&key" $ do
    it "basic keyword args" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 0)) (add x y))))"
        , "  (print (int-to-str (f &key x 10 &key y 20))))"
        ]) >>= (`shouldBe` "30")

    it "keyword args reordered" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 0)) (sub x y))))"
        , "  (print (int-to-str (f &key y 3 &key x 10))))"
        ]) >>= (`shouldBe` "7")

    it "keyword args with defaults" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 100)) (add x y))))"
        , "  (print (int-to-str (f &key x 5))))"
        ]) >>= (`shouldBe` "105")

    it "all keyword args defaulted" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 1) (y 2)) (add x y))))"
        , "  (print (int-to-str (f))))"
        ]) >>= (`shouldBe` "3")

    it "keyword with required params" $
      run (T.unlines
        [ "(let ((f (lam (a &key (x 0)) (add a x))))"
        , "  (print (int-to-str (f 10 &key x 5))))"
        ]) >>= (`shouldBe` "15")

  describe "typeclasses" $ do
    it "basic class + instance + method call" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (show 42))"
        ]) >>= (`shouldBe` "42")

    it "class method with multiple args" $
      run (T.unlines
        [ "(cls EQUAL (a) (equal %a %a %BOOL))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eq x y))))"
        , "(print (int-to-str (if (equal 1 1) 1 0)))"
        ]) >>= (`shouldBe` "1")

    it "multiple instances of same class" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) x)))"
        , "(print (show 42))"
        ]) >>= (`shouldBe` "42")

    it "different instances dispatched by type" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) (concat \"[\" (concat x \"]\")))))"
        , "(print (show \"hi\"))"
        ]) >>= (`shouldBe` "[hi]")

    it "class with multiple methods" $
      run (T.unlines
        [ "(cls EQUAL (a)"
        , "  (equal %a %a %BOOL)"
        , "  (nequal %a %a %BOOL))"
        , "(inst EQUAL %INT"
        , "  (equal (lam ((x %INT) (y %INT)) (eq x y)))"
        , "  (nequal (lam ((x %INT) (y %INT)) (not (eq x y)))))"
        , "(print (int-to-str (if (nequal 1 2) 1 0)))"
        ]) >>= (`shouldBe` "1")

    it "class method used in let binding" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((result (show 99)))"
        , "  (print result))"
        ]) >>= (`shouldBe` "99")

    it "class method in if branches" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (if TRUE (show 1) (show 2)))"
        ]) >>= (`shouldBe` "1")

    it "instance for float type" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %FLT (show (lam ((x %FLT)) (flt-to-str x))))"
        , "(print (show 3.14))"
        ]) >>= (`shouldBe` "3.14")

    it "method used with ADT" $
      run (T.unlines
        [ "(TYPE Color () (Red) (Green) (Blue))"
        , "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %Color (show (lam ((c %Color)) (case c"
        , "  ((Red) \"red\")"
        , "  ((Green) \"green\")"
        , "  ((Blue) \"blue\")))))"
        , "(print (show Red))"
        ]) >>= (`shouldBe` "red")

    it "method result passed to another function" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (concat \"val=\" (show 7)))"
        ]) >>= (`shouldBe` "val=7")

    it "class method in nested let" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((a (show 1)))"
        , "  (let ((b (show 2)))"
        , "    (print (concat a b))))"
        ]) >>= (`shouldBe` "12")

    it "two different classes" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(cls EQUAL (a) (equal %a %a %BOOL))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eq x y))))"
        , "(print (concat (show 42) (if (equal 1 1) \" yes\" \" no\")))"
        ]) >>= (`shouldBe` "42 yes")

    it "polymorphic function with class method" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((print-show (lam (x) (print (show x)))))"
        , "  (print-show 42))"
        ]) >>= (`shouldBe` "42")

    it "polymorphic function called multiple times same type" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((to-str (lam (x) (show x))))"
        , "  (let ((_ (print (to-str 1))))"
        , "    (print (to-str 2))))"
        ]) >>= (`shouldBe` "1\n2")

    it "polymorphic function called with different types" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) (concat \"[\" (concat x \"]\")))))"
        , "(let ((display (lam (x) (show x))))"
        , "  (let ((_ (print (display 42))))"
        , "    (print (display \"hi\"))))"
        ]) >>= (`shouldBe` "42\n[hi]")

    it "polymorphic function with multi-method class" $
      run (T.unlines
        [ "(cls EQUAL (a)"
        , "  (equal %a %a %BOOL)"
        , "  (nequal %a %a %BOOL))"
        , "(inst EQUAL %INT"
        , "  (equal (lam ((x %INT) (y %INT)) (eq x y)))"
        , "  (nequal (lam ((x %INT) (y %INT)) (not (eq x y)))))"
        , "(let ((check (lam (x y) (if (equal x y) (print \"eq\") (print \"neq\")))))"
        , "  (check 1 1))"
        ]) >>= (`shouldBe` "eq")

    it "polymorphic with multiple classes" $
      run (T.unlines
        [ "(cls SHOW (a) (show %a %STR))"
        , "(cls EQUAL (a) (equal %a %a %BOOL))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eq x y))))"
        , "(let ((show-eq (lam (x y)"
        , "  (if (equal x y) (show x) (concat (show x) (concat \" != \" (show y)))))))"
        , "  (print (show-eq 1 1)))"
        ]) >>= (`shouldBe` "1")

    it "polymorphic function with ADT instance" $
      run (T.unlines
        [ "(TYPE Color () (Red) (Green) (Blue))"
        , "(cls SHOW (a) (show %a %STR))"
        , "(inst SHOW %Color (show (lam ((c %Color)) (case c"
        , "  ((Red) \"red\") ((Green) \"green\") ((Blue) \"blue\")))))"
        , "(let ((display (lam (x) (print (show x)))))"
        , "  (display Red))"
        ]) >>= (`shouldBe` "red")

-- HELPERS

pipeline :: T.Text -> IO T.Text
pipeline src = do
  preludeDecls <- Stdlib.loadPrelude
  let sexprs = case Parser.parseSExprs "<test>" src of
        Left e -> error ("parse error: " ++ show e)
        Right s -> s
      expanded = case MacroExpand.expand sexprs of
        Left e -> error ("macro error: " ++ e)
        Right s -> s
      prog = case SExpr.toProgram expanded of
        Left e -> error ("sexpr error: " ++ SExpr.ceMsg e)
        Right p -> p
      exprs = preludeDecls ++ CST.progExprs prog
  case Resolve.resolve S.empty exprs of
    Left e -> error ("resolve error: " ++ show e)
    Right resolved -> case TC.typecheck M.empty resolved of
      Left e -> error ("typecheck error: " ++ show e)
      Right typed ->
        pure $ Codegen.codegen (LL.lambdaLift (CC.closureConvert typed))

run :: T.Text -> IO String
run = runWith []

runWith :: [String] -> T.Text -> IO String
runWith = runWithStdin ""

runWithInput :: String -> T.Text -> IO String
runWithInput stdin' = runWithStdin stdin' []

runWithStdin :: String -> [String] -> T.Text -> IO String
runWithStdin stdin' extraArgs src = do
  ir <- pipeline src
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode
        "/tmp/pllisp_test_exe" extraArgs stdin'
      case ec2 of
        ExitSuccess   -> pure (strip out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)
  where
    strip = reverse . dropWhile (== '\n') . reverse

-- | Compile a module + main source and run the result.
-- modName: uppercase module name, modSrc: module body, unquals: unqualified imports, mainSrc: main body
runWithModule :: CST.Symbol -> T.Text -> [CST.Symbol] -> T.Text -> IO String
runWithModule modName modSrc unquals mainSrc = do
  ir <- importPipeline modName modSrc unquals mainSrc
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode "/tmp/pllisp_test_exe" [] ""
      case ec2 of
        ExitSuccess   -> pure (reverse . dropWhile (== '\n') . reverse $ out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

importPipeline :: CST.Symbol -> T.Text -> [CST.Symbol] -> T.Text -> IO T.Text
importPipeline modName modSrc unquals mainSrc = do
  preludeDecls <- Stdlib.loadPrelude
  -- Compile module
  let modTyped = case Parser.parseProgram "<mod>" modSrc of
        Left e -> error ("mod parse: " ++ show e)
        Right prog ->
          let exprs = preludeDecls ++ Mod.desugarTopLevel (CST.progExprs prog)
          in case Resolve.resolve S.empty exprs of
            Left e -> error ("mod resolve: " ++ show e)
            Right resolved -> case TC.typecheck M.empty resolved of
              Left e -> error ("mod typecheck: " ++ show e)
              Right typed -> typed
      modExports = Mod.collectExports modTyped
      exportMap = M.singleton modName modExports
      imports = [CST.Import modName unquals]
      (resolveScope, tcCtx, normMap) = Mod.buildImportScope exportMap imports
  case Parser.parseProgram "<main>" mainSrc of
    Left e -> error ("main parse: " ++ show e)
    Right prog ->
      let exprs = preludeDecls ++ Mod.desugarTopLevel (CST.progExprs prog)
      in case Resolve.resolveWith resolveScope normMap exprs of
        Left e -> error ("main resolve: " ++ show e)
        Right resolved -> case TC.typecheck tcCtx resolved of
          Left e -> error ("main typecheck: " ++ show e)
          Right typed ->
            let merged = Mod.mergeImportedCode [modTyped] typed
            in pure $ Codegen.codegen (LL.lambdaLift (CC.closureConvert merged))
